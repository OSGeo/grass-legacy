/*  gs_query.c
    Bill Brown, USACERL  
    January 1994
*/
	
#include "gstypes.h"
#include <math.h>
#include <stdio.h>

/***********************************************************************/
/* Crude method of intersecting line of sight with closest part of surface. 
   Uses los vector to determine the point of first intersection
   which is returned in point. Returns 0 if los doesn't intersect.  */
/* los should ALREADY be in surf-world coordinates */

int
gs_los_intersect(surfid, los, point)
int surfid;
float 	los[2][3], point[3];
{
    float dx, dy, dz, u_d[3];
    float a[3], incr, min_incr, tlen, len;
    int   outside, above, below, edge, istep;
    float b[3], sx, sy, sz;
    geosurf *gs;
    typbuff *buf;

    if(NULL == (gs = gs_get_surf(surfid))) return(0);

    if(0 == GS_v3dir(los[FROM], los[TO], u_d)) return(0);
    buf = gs_get_att_typbuff(gs, ATT_TOPO, 0);

    istep = edge = below = 0;

    len = 0.0;
    tlen = GS_distance (los[FROM], los[TO]);
    GS_get_longdim(&incr);
    incr /= 1000.;           
    min_incr = incr/100.;


    dx = incr * u_d[X];
    dy = incr * u_d[Y];
    dz = incr * u_d[Z];

    a[X] = los[FROM][X];
    a[Y] = los[FROM][Y];
    a[Z] = los[FROM][Z];

    b[X] = a[X] - gs->x_trans;
    b[Y] = a[Y] - gs->y_trans;

    if(viewcell_tri_interp(gs, buf, b, 0)){ /* expects surface coords */
	b[Z] += gs->z_trans; 
	if ( a[Z] < b[Z] ){   /*  viewing from below surface  */
/*    don't use this method 
fprintf(stderr,"view from below\n");
	    below = 1;
*/
	    return (0);  
	}
    }

    while (incr > min_incr){
	outside = 0;
	above = 0;
	b[X] = a[X] - gs->x_trans;
	b[Y] = a[Y] - gs->y_trans;
	if(viewcell_tri_interp(gs, buf, b, 0)){ /* ignores masks */
	    b[Z] += gs->z_trans; 
	    above = ( a[Z] > b[Z] );
	}
	else{ 
	    outside = 1;
	    if (istep > 3){ edge = 1; below = 1;}
	}

	while (outside || above){
	    a[X] += dx;
	    a[Y] += dy;
	    a[Z] += dz;
	    len += incr;
	    outside = 0;
	    above = 0;
	    b[X] = a[X] - gs->x_trans;
	    b[Y] = a[Y] - gs->y_trans;
	    if(viewcell_tri_interp(gs, buf, b, 0)){
		b[Z] += gs->z_trans; 
		above = ( a[Z] > b[Z] );
/*
fprintf(stderr,"trace[Z] = %f, surf[Z] = %f, above = %d\n", a[Z],b[Z],above);
fprintf(stderr,"LOS: %f,%f,%f SURF: %f,%f,%f\n", a[X],a[Y],a[Z],b[X],b[Y],b[Z]);
*/
	    }
	    else 
		outside = 1;

	    if ( len > tlen ){
/*
fprintf(stderr,"looking over surface\n");
*/
		return 0;  /* over surface *//* under surface */
	    }
	}
	a[X] -= dx;
	a[Y] -= dy;
	a[Z] -= dz;
	incr /= 2.0;
	++ istep;
	dx = incr * u_d[X];
	dy = incr * u_d[Y];
	dz = incr * u_d[Z];
    }

    if ((edge) && (b[Z] - (a[Z]+dz*2.0) > incr * u_d[Z])){
/*
fprintf(stderr,"looking under surface\n");
*/
	return 0;  
    }

    point[X] = b[X];
    point[Y] = b[Y];
    point[Z] = b[Z] - gs->z_trans;

    return (1);

}


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
/*
pr_plane(ph_num);
fprintf(stderr,", t = %.2lf, vn = %.2lf, vd = %.2lf\n", t, vn, vd);
*/
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
/* DEBUG */
pr_plane(pnum)
{

    switch(pnum){
	case 0:
	    fprintf(stderr,"top plane");
	    break;
	case 1:
	    fprintf(stderr,"bottom plane");
	    break;
	case 2:
	    fprintf(stderr,"left plane");
	    break;
	case 3:
	    fprintf(stderr,"right plane");
	    break;
	case 4:
	    fprintf(stderr,"front plane");
	    break;
	case 5:
	    fprintf(stderr,"back plane");
	    break;
	default:
	    fprintf(stderr,"clipping plane %d", 6 - pnum);
	    break;
    }
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
    /*
    GS_get_region(&n, &s, &w, &e);
    */

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
/*
fprintf(stderr,"Ray missed polyhedron\n");
*/
	return(0);
    }
    if(ret == FRONTFACE){
/*
fprintf(stderr,"Ray hit polyhedron front, plane %d\n",retp);
*/
	GS_v3mult(dir, (float)dist);
	GS_v3add(los[FROM],dir);
    }
/*
else
fprintf(stderr,"Ray hit polyhedron back, plane %d\n",retp);

fprintf(stderr,"%f %f %f %f\n",planes[retp][X],planes[retp][Y],
			       planes[retp][Z],planes[retp][W]);
*/

    return(1);
}


/***********************************************************************/
/***********************************************************************/

