/* Functions for querying of 3D surface data 
 * Bill Brown, UI-GMSL
 * updated Spring 1997
 */
	
#include "gstypes.h"
#include <math.h>
#include <stdio.h>


/************************************************************************/

int 
GS_get_selected_point_on_surface(sx, sy, id, x, y, z)
int sx, sy;
int *id;
float *x, *y, *z;
{
float los[2][3], los2[2][3], find_dist[MAX_SURFS], closest;
Point3 point, tmp, finds[MAX_SURFS];
int surfs[MAX_SURFS], i, iclose, numhits=0;
float scalx, scaly, scalz;
geosurf *gs;

    gsd_get_los(los, (short)sx, (short)sy);
    /* returns surface-world coords */

    if(!gs_setlos_enterdata(los)) return(0);

    for(i=0; i<Next_surf; i++){
	gs = gs_get_surf(Surf_ID[i]);

        /* los_intersect expects surf-world coords (xy transl, no scaling) */

	if(gs_los_intersect(Surf_ID[i], los, point)){ /* returns surf-world */
	    if(!gs_point_is_masked(gs, point)){
		GS_v3eq(tmp, point);
		tmp[X] += gs->x_trans;
		tmp[Y] += gs->y_trans;
		tmp[Z] += gs->z_trans;
		find_dist[numhits] = GS_distance (los[FROM], tmp);
		gsd_surf2real(gs, point);
		GS_v3eq(finds[numhits], point);
		surfs[numhits] = Surf_ID[i];
		numhits++;
	    }
	}
    }

    for(i = iclose = 0; i<numhits; i++){
	closest = find_dist[iclose];
	if(find_dist[i] < closest) iclose=i;
    }

    if(numhits){
	*x = finds[iclose][X];
	*y = finds[iclose][Y];
	*z = finds[iclose][Z];
	*id = surfs[iclose];
    }

    return(numhits);

}

/***********************************************************************/
/* Returns FROM-TO vector in surf-world coordinates, clipped to front
 * and back view planes 
*/

gsd_get_los(vect, sx, sy)
float vect[2][3];
short sx, sy;       /* screen coordinates */
{
static int first;

double fx, fy, fz, tx, ty, tz;
GLdouble modelMatrix[16], projMatrix[16];
GLint viewport[4];

    glPushMatrix();

    gsd_do_scale(1); /* does scaling & translation, including Z exag */
    glGetDoublev(GL_MODELVIEW_MATRIX, modelMatrix);
    glGetDoublev(GL_PROJECTION_MATRIX, projMatrix);
    glGetIntegerv(GL_VIEWPORT, viewport);
    glPopMatrix();

    gluUnProject((GLdouble)sx, (GLdouble)sy, 0.0, modelMatrix, 
		projMatrix, viewport, &fx, &fy, &fz);
    gluUnProject((GLdouble)sx, (GLdouble)sy, 1.0, modelMatrix, 
		projMatrix, viewport, &tx, &ty, &tz);

    vect[FROM][X] = fx;
    vect[FROM][Y] = fy;
    vect[FROM][Z] = fz;
    vect[TO][X] = tx;
    vect[TO][Y] = ty;
    vect[TO][Z] = tz;


    return(1);


}

/************************************************************************/

gsd_do_scale(doexag)
int doexag;
{
float sx, sy, sz;
float min, max;
    
    GS_get_scale(&sx, &sy, &sz, doexag);    
    gsd_scale(sx, sy, sz);
    GS_get_zrange(&min, &max, 0);
    gsd_translate(0.0,0.0,-min);
}

/************************************************************************/

/***********************************************************************/
/* Method of intersecting line of sight with closest part of surface. 
 * Uses los vector to determine the point of first intersection
 * which is returned in point. Returns 0 if los doesn't intersect. 
 * LOS should ALREADY be in surf-world coordinates - as returned by
 * gsd_get_los.
 * This version uses the shadow of the los projected down to
 * the surface to generate a line_on_surf, then follows each
 * point in that line until the los intersects it.
*/

int
gs_los_intersect(surfid, los, point)
int surfid;
float 	los[2][3], point[3];
{
    double incr, min_incr;
    float p1, p2, u_d[3];
    int   above, below, ret, num, i, usedx;
    float a[3], b[3], sx, sy, sz;
    float bgn[3], end[3], a1[3];
    geosurf *gs;
    typbuff *buf;
    Point3 *points;

    if(NULL == (gs = gs_get_surf(surfid))) return(0);

    if(0 == GS_v3dir(los[FROM], los[TO], u_d)) return(0);
    buf = gs_get_att_typbuff(gs, ATT_TOPO, 0);

    GS_v3eq(bgn, los[FROM]);
    GS_v3eq(end, los[TO]);

    bgn[X] -= gs->x_trans;
    bgn[Y] -= gs->y_trans;

    end[X] -= gs->x_trans;
    end[Y] -= gs->y_trans;

    /* need translate? */

    points = gsdrape_get_allsegments(gs, bgn, end, &num);

    if (num<2)
	return (0);
    
    /* use larger of deltas for better precision */
    usedx = ( fabs(u_d[X]) > fabs (u_d[Y]) );
    if(usedx)
	incr = ((points[0][X] - (los[FROM][X] - gs->x_trans))/u_d[X]);
    else if(u_d[Y])
	incr = ((points[0][Y] - (los[FROM][Y] - gs->y_trans))/u_d[Y]);
    else{
	point[X] = los[FROM][X] - gs->x_trans;
	point[Y] = los[FROM][Y] - gs->y_trans;
	return(viewcell_tri_interp(gs, buf, point, 1));
    }

    /* This should bring us right above (or below) the first point */
    a[X] = los[FROM][X] + incr * u_d[X] - gs->x_trans;
    a[Y] = los[FROM][Y] + incr * u_d[Y] - gs->y_trans;
    a[Z] = los[FROM][Z] + incr * u_d[Z] - gs->z_trans;

    if ( a[Z] < points[0][Z] ){     /*  viewing from below surface  */
				    /*  don't use this method */
	return (0);  
    }

    GS_v3eq(a1, a);
    GS_v3eq(b, a);
    for (i=1; i < num; i++){
	if(usedx) 
	    incr = ((points[i][X] - a1[X])/u_d[X]);
	else 
	    incr = ((points[i][Y] - a1[Y])/u_d[Y]);

	a[X] = a1[X] + (incr * u_d[X]) ;
	a[Y] = a1[Y] + (incr * u_d[Y]) ;
	a[Z] = a1[Z] + (incr * u_d[Z]) ;
	above = ( a[Z] >= points[i][Z] );
	if(above){
	    GS_v3eq(b, a);
	    continue;
	}
	/* 
	 * Now we know b[Z] is above points[i-1] 
	 * and a[Z] is below points[i]
	 * Since there should only be one polygon along this seg,
	 * just interpolate to intersect 
	 */
	
	if(usedx)
	    incr = ((a[X] - b[X])/u_d[X]);
	else
	    incr = ((a[Y] - b[Y])/u_d[Y]);
	if ( 1 == (ret = segs_intersect( 1.0, points[i][Z], 
				  0.0, points[i-1][Z], 
				  1.0, a[Z], 
				  0.0, b[Z], &p1, &p2))){
	    point[X] = points[i-1][X] + (u_d[X]*incr*p1);
	    point[Y] = points[i-1][Y] + (u_d[Y]*incr*p1);
	    point[Z] = p2;

	    return (1);
	}
	fprintf(stderr,"line of sight error %d\n", ret);
	return 0;  

    }

    /* over surface */
    return 0;  

}


/***********************************************************************/
gs_get_databounds_planes(planes)
Point4 planes[];
{
float n,s,w,e,b,t;
Point3 tlfront, brback;

    GS_get_zrange(&b, &t, 0);  
    gs_get_xrange(&w, &e);
    gs_get_yrange(&s, &n);

    tlfront[X] = tlfront[Y] = 0.0;
    tlfront[Z] = t;
 
    brback[X] = e - w;
    brback[Y] = n - s;
    brback[Z] = b;

    /* top */
    planes[0][X] = planes[0][Y] = 0.0;
    planes[0][Z] = 1.0;
    planes[0][W] = -(DOT3(planes[0], tlfront));
    
    /* bottom */
    planes[1][X] = planes[1][Y] = 0.0;
    planes[1][Z] = -1.0;
    planes[1][W] = -(DOT3(planes[1], brback));
    
    /* left */
    planes[2][Y] = planes[2][Z] = 0.0;
    planes[2][X] = -1.0;
    planes[2][W] = -(DOT3(planes[2], tlfront));
    
    /* right */
    planes[3][Y] = planes[3][Z] = 0.0;
    planes[3][X] = 1.0;
    planes[3][W] = -(DOT3(planes[3], brback));

    /* front */
    planes[4][X] = planes[4][Z] = 0.0;
    planes[4][Y] = -1.0;
    planes[4][W] = -(DOT3(planes[4], tlfront));
    
    /* back */
    planes[5][X] = planes[5][Z] = 0.0;
    planes[5][Y] = 1.0;
    planes[5][W] = -(DOT3(planes[5], brback));
    
}


/***********************************************************************/
/* Gets all current cutting planes & data bounding planes, intersects 
   los with resulting convex polyhedron, then replaces los[FROM] with
   first point on ray inside data.
*/
gs_setlos_enterdata(los)
Point3 los[2];
{
Point4 planes[12];  /* MAX_CPLANES + 6  - should define this */
Point3 dir;
double dist, maxdist;
int num, ret, retp; /* might want to tell if retp is a clipping plane */

    gs_get_databounds_planes(planes);
    num = gsd_get_cplanes(planes+6);
    GS_v3dir(los[FROM], los[TO], dir);
    maxdist = GS_distance(los[FROM], los[TO]);
    
    ret = RayCvxPolyhedronInt(los[0], dir, maxdist, 
	    planes, num+6, &dist, &retp);

    if(ret == MISSED){
	return(0);
    }
    if(ret == FRONTFACE){
	GS_v3mult(dir, (float)dist);
	GS_v3add(los[FROM],dir);
    }

    return(1);
}

/***********************************************************************/

/***********************************************************************/
/* Ray-Convex Polyhedron Intersection Test 
 * originally by Eric Haines, erich@eye.com
 *
 * This test checks the ray against each face of a polyhedron, checking whether
 * the set of intersection points found for each ray-plane intersection
 * overlaps the previous intersection results.  If there is no overlap (i.e.
 * no line segment along the ray that is inside the polyhedron), then the
 * ray misses and returns 0; else 1 is returned if the ray is entering the
 * polyhedron, -1 if the ray originates inside the polyhedron.  If there is
 * an intersection, the distance and the nunber of the face hit is returned.
 */

#ifndef	HUGE_VAL
#define	HUGE_VAL	1.7976931348623157e+308
#endif

/* return codes */
#define	MISSED		 0
#define	FRONTFACE	 1
#define	BACKFACE	-1

int	RayCvxPolyhedronInt( org, dir, tmax, phdrn, ph_num, tresult, pn )
Point3	org, dir ;	/* origin and direction of ray */
double	tmax ;		/* maximum useful distance along ray */
Point4	phdrn[] ;	        /* list of planes in convex polyhedron */
int	ph_num ;	/* number of planes in convex polyhedron */
double	*tresult ;	/* returned: distance of intersection along ray */
int	*pn ;		/* returned: number of face hit (0 to ph_num-1) */
{
double	tnear, tfar, t, vn, vd ;
int	fnorm_num, bnorm_num ;	/* front/back face # hit */


    tnear = -HUGE_VAL ;
    tfar = tmax ;

    /* Test each plane in polyhedron */
    for ( ; ph_num-- ; ) {
	/* Compute intersection point T and sidedness */
	vd = DOT3( dir, phdrn[ph_num] ) ;
	vn = DOT3( org, phdrn[ph_num] ) + phdrn[ph_num][W] ;
	if ( vd == 0.0 ) {
	    /* ray is parallel to plane - check if ray origin is inside plane's
	       half-space */
	    if ( vn > 0.0 ){
		/* ray origin is outside half-space */
		return ( MISSED ) ;
	    }
	} else {
	    /* ray not parallel - get distance to plane */
	    t = -vn / vd ;

	    if ( vd < 0.0 ) {
		/* front face - T is a near point */
		if ( t > tfar ) return ( MISSED ) ;
		if ( t > tnear ) {
		    /* hit near face, update normal */
		    fnorm_num = ph_num ;
		    tnear = t ;
		}
	    } else {
		/* back face - T is a far point */
		if ( t < tnear ) return ( MISSED ) ;
		if ( t < tfar ) {
		    /* hit far face, update normal */
		    bnorm_num = ph_num ;
		    tfar = t ;
		}
	    }
	}
    }

    /* survived all tests */
    /* Note: if ray originates on polyhedron, may want to change 0.0 to some
     * epsilon to avoid intersecting the originating face.
     */
    if ( tnear >= 0.0 ) {
	/* outside, hitting front face */
	*tresult = tnear ;
	*pn = fnorm_num;
	return ( FRONTFACE ) ;
    } else {
	if ( tfar < tmax ) {
	    /* inside, hitting back face */
	    *tresult = tfar ;
	    *pn = bnorm_num;
	    return ( BACKFACE ) ;
	} else {
	    /* inside, but back face beyond tmax */
	    return ( MISSED ) ;
	}
    }
}
/***********************************************************************/


