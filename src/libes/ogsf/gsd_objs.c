/*
* $Id$
*/

/*  gsd.c
    Bill Brown, USACERL  
    October 1993
*/
	
#include <stdio.h>
#include <stdlib.h>

#include "gstypes.h"
#include "gsget.h"
#include "math.h"
#include "rowcol.h"

/*
#define TRACE_DFUNCS
*/

static void init_stuff(void);

/* vertices for octohedron */
float Octo[6][3] = {
    { 1.0,  0.0,  0.0},
    { 0.0,  1.0,  0.0},
    { 0.0,  0.0,  1.0},
    {-1.0,  0.0,  0.0},
    { 0.0, -1.0,  0.0},
    { 0.0,  0.0, -1.0}
};

#define ONORM .57445626

/* normals for flat-shaded octohedron */
float OctoN[8][3] = {
    { ONORM, ONORM, ONORM},
    { -ONORM, ONORM, ONORM},
    { ONORM, -ONORM, ONORM},
    { -ONORM, -ONORM, ONORM},
    { ONORM, ONORM, -ONORM},
    { -ONORM, ONORM, -ONORM},
    { ONORM, -ONORM, -ONORM},
    { -ONORM, -ONORM, -ONORM},
};

float origin[3] = { 0.0, 0.0, 0.0 };

#define UP_NORM Octo[2]
#define DOWN_NORM Octo[5]
#define ORIGIN origin

/* vertices & normals for octagon in xy plane */
float ogverts[8][3];

/* vertices for octagon in xy plane, z=1 */
float ogvertsplus[8][3];

float Pi;

static void init_stuff(void)
{
    float cos45;
    int i;
    static int first=1;

    if (first)
    {
	first=0;
        
	cos45 = cos(atan(1.0));

	for (i=0; i<8; i++)
	{
	    ogverts[i][Z] = 0.0;
	    ogvertsplus[i][Z] = 1.0;
	}

	ogverts[0][X] = ogvertsplus[0][X] = 1.0;
	ogverts[0][Y] = ogvertsplus[0][Y] = 0.0;
	ogverts[1][X] = ogvertsplus[1][X] = cos45;
	ogverts[1][Y] = ogvertsplus[1][Y] = cos45;
	ogverts[2][X] = ogvertsplus[2][X] = 0.0;
	ogverts[2][Y] = ogvertsplus[2][Y] = 1.0;
	ogverts[3][X] = ogvertsplus[3][X] = -cos45;
	ogverts[3][Y] = ogvertsplus[3][Y] = cos45;
	ogverts[4][X] = ogvertsplus[4][X] = -1.0;
	ogverts[4][Y] = ogvertsplus[4][Y] = 0.0;
	ogverts[5][X] = ogvertsplus[5][X] = -cos45;
	ogverts[5][Y] = ogvertsplus[5][Y] = -cos45;
	ogverts[6][X] = ogvertsplus[6][X] = 0.0;
	ogverts[6][Y] = ogvertsplus[6][Y] = -1.0;
	ogverts[7][X] = ogvertsplus[7][X] = cos45;
	ogverts[7][Y] = ogvertsplus[7][Y] = -cos45;

	Pi = 4.0 * atan(1.0);
    }

    return;
}

void gsd_plus(float *center, int colr, float siz)
{
    float v1[3], v2[3];

    gsd_color_func(colr);
    siz *= .5;
   
    v1[Z] = v2[Z] = center[Z];

    v1[X] = v2[X] = center[X];
    v1[Y] = center[Y] - siz;
    v2[Y] = center[Y] + siz;
    gsd_bgnline();
	gsd_vert_func(v1);
	gsd_vert_func(v2);
    gsd_endline();

    v1[Y] = v2[Y] = center[Y];
    v1[X] = center[X] - siz;
    v2[X] = center[X] + siz;
    gsd_bgnline();
	gsd_vert_func(v1);
	gsd_vert_func(v2);
    gsd_endline();

    return;
}

/* TODO: remove fudge, instead fudge the Z buffer */ 
void gsd_line_onsurf(geosurf *gs, float *v1, float *v2)
{
    int i, np;
    Point3 *pts;
    float fudge;

    pts = gsdrape_get_segments(gs, v1, v2, &np);
    if (pts)
    {
	fudge = FUDGE(gs);
	gsd_bgnline();
	
	for (i=0; i<np; i++)
	{
	    pts[i][Z] += fudge;
	    gsd_vert_func(pts[i]);
	}
	
	gsd_endline();

	/* fix Z values? */
	v1[Z] = pts[0][Z];
	v2[Z] = pts[np-1][Z];
    }

    return;
}

/* TODO: remove fudge, instead fudge the Z buffer */ 
/* Like above, except only draws first n points of line, or np,
 * whichever is less.  Returns number of points used. Fills
 * pt with last pt drawn.
*/
int gsd_nline_onsurf(geosurf *gs, float *v1, float *v2, float *pt, int n)
{
    int i, np, pdraw;
    Point3 *pts;
    float fudge;

    pts = gsdrape_get_segments(gs, v1, v2, &np);
    
    if (pts)
    {
	pdraw = n < np ? n : np;
	fudge = FUDGE(gs);
	gsd_bgnline();
	
	for (i=0; i<pdraw; i++)
	{
	    pts[i][Z] += fudge;
	    gsd_vert_func(pts[i]);
	}
	
	gsd_endline();

	pt[X] = pts[i-1][X];
	pt[Y] = pts[i-1][Y];

	/* fix Z values? */
	v1[Z] = pts[0][Z];
	v2[Z] = pts[np-1][Z];

	return(i);
    }
    
    return(0);
}

/* Note gs: NULL if flat */
void gsd_x(geosurf *gs, float *center, int colr, float siz)
{
    float v1[3], v2[3];

    gsd_color_func(colr);
    siz *= .5;
   
    v1[Z] = v2[Z] = center[Z];

    v1[X] = center[X] - siz;
    v2[X] = center[X] + siz;
    v1[Y] = center[Y] - siz;
    v2[Y] = center[Y] + siz;

    if (gs)
    {
	gsd_line_onsurf(gs, v1, v2);
    }
    else
    {
	gsd_bgnline();
	    gsd_vert_func(v1);
	    gsd_vert_func(v2);
	gsd_endline();
    }

    v1[X] = center[X] - siz;
    v2[X] = center[X] + siz;
    v1[Y] = center[Y] + siz;
    v2[Y] = center[Y] - siz;

    if (gs)
    {
	gsd_line_onsurf(gs, v1, v2);
    }
    else
    {
	gsd_bgnline();
	    gsd_vert_func(v1);
	    gsd_vert_func(v2);
	gsd_endline();
    }

    return;
}

void gsd_diamond(float *center, int colr, float siz)
{
    int preshade;

    /* seems right, but isn't
    	siz *= .5;
    */
    
    gsd_pushmatrix();
    gsd_translate(center[X], center[Y], center[Z]);
    gsd_scale(siz, siz, siz);
    preshade = gsd_getshademodel();
    gsd_shademodel(0);  /* want flat shading */

    gsd_bgnpolygon();
	gsd_litvert_func(OctoN[0], colr, Octo[0]);
	gsd_litvert_func(OctoN[0], colr, Octo[1]);
	gsd_litvert_func(OctoN[0], colr, Octo[2]);
    gsd_endpolygon();
    
    gsd_bgnpolygon();
	gsd_litvert_func(OctoN[1], colr, Octo[2]);
	gsd_litvert_func(OctoN[1], colr, Octo[1]);
	gsd_litvert_func(OctoN[1], colr, Octo[3]);
    gsd_endpolygon();
    
    gsd_bgnpolygon();
	gsd_litvert_func(OctoN[2], colr, Octo[2]);
	gsd_litvert_func(OctoN[2], colr, Octo[4]);
	gsd_litvert_func(OctoN[2], colr, Octo[0]);
    gsd_endpolygon();
    
    gsd_bgnpolygon();
	gsd_litvert_func(OctoN[3], colr, Octo[2]);
	gsd_litvert_func(OctoN[3], colr, Octo[3]);
	gsd_litvert_func(OctoN[3], colr, Octo[4]);
    gsd_endpolygon();

    gsd_bgnpolygon();
	gsd_litvert_func(OctoN[4], colr, Octo[0]);
	gsd_litvert_func(OctoN[4], colr, Octo[5]);
	gsd_litvert_func(OctoN[4], colr, Octo[1]);
    gsd_endpolygon();
    
    gsd_bgnpolygon();
	gsd_litvert_func(OctoN[5], colr, Octo[1]);
	gsd_litvert_func(OctoN[5], colr, Octo[5]);
	gsd_litvert_func(OctoN[5], colr, Octo[3]);
    gsd_endpolygon();
    
    gsd_bgnpolygon();
	gsd_litvert_func(OctoN[6], colr, Octo[5]);
	gsd_litvert_func(OctoN[6], colr, Octo[0]);
	gsd_litvert_func(OctoN[6], colr, Octo[4]);
    gsd_endpolygon();
    
    gsd_bgnpolygon();
	gsd_litvert_func(OctoN[7], colr, Octo[5]);
	gsd_litvert_func(OctoN[7], colr, Octo[4]);
	gsd_litvert_func(OctoN[7], colr, Octo[3]);
    gsd_endpolygon();

    #ifdef OCT_SHADED
    {
	gsd_bgntmesh();
            gsd_litvert_func(Octo[0], colr, Octo[0]);
            gsd_litvert_func(Octo[1], colr, Octo[1]);
	gsd_swaptmesh();
            gsd_litvert_func(Octo[2], colr, Octo[2]);
	gsd_swaptmesh();
            gsd_litvert_func(Octo[4], colr, Octo[4]);
	gsd_swaptmesh();
            gsd_litvert_func(Octo[5], colr, Octo[5]);
	gsd_swaptmesh();
            gsd_litvert_func(Octo[1], colr, Octo[1]);
            gsd_litvert_func(Octo[3], colr, Octo[3]);
            gsd_litvert_func(Octo[2], colr, Octo[2]);
	gsd_swaptmesh();
            gsd_litvert_func(Octo[4], colr, Octo[4]);
	gsd_swaptmesh();
            gsd_litvert_func(Octo[5], colr, Octo[5]);
	gsd_swaptmesh();
            gsd_litvert_func(Octo[1], colr, Octo[1]);
	gsd_endtmesh();
    }
    #endif

    gsd_popmatrix();
    gsd_shademodel(preshade);

    return;
}

void gsd_diamond_lines(void)
{
    gsd_bgnline();
    gsd_vert_func(Octo[0]);
    gsd_vert_func(Octo[3]);
    gsd_endline();

    gsd_bgnline();
    gsd_vert_func(Octo[1]);
    gsd_vert_func(Octo[4]);
    gsd_endline();

    gsd_bgnline();
    gsd_vert_func(Octo[2]);
    gsd_vert_func(Octo[5]);
    gsd_endline();

    return;
}

void gsd_drawsphere(float *center, unsigned long colr, float siz)
{
    siz *= .5;  /* siz is diameter, gsd_sphere uses radius */
    gsd_color_func(colr);
    gsd_sphere(center, siz);

    return;
}

void gsd_draw_asterisk(float *center, unsigned long colr, float siz)
{
    float angle;

    angle = 45.;  /* degrees */

    gsd_pushmatrix();
    gsd_translate(center[X], center[Y], center[Z]);
    gsd_scale(siz, siz, siz);
    gsd_color_func(colr);

    gsd_diamond_lines();

    gsd_pushmatrix();
    gsd_rot(angle, 'x');
    gsd_diamond_lines();
    gsd_popmatrix();

    gsd_pushmatrix();
    gsd_rot(-angle, 'x');
    gsd_diamond_lines();
    gsd_popmatrix();

    gsd_pushmatrix();
    gsd_rot(angle, 'y');
    gsd_diamond_lines();
    gsd_popmatrix();

    gsd_pushmatrix();
    gsd_rot(-angle, 'y');
    gsd_diamond_lines();
    gsd_popmatrix();

    gsd_pushmatrix();
    gsd_rot(angle, 'z');
    gsd_diamond_lines();
    gsd_popmatrix();

    gsd_pushmatrix();
    gsd_rot(-angle, 'z');
    gsd_diamond_lines();
    gsd_popmatrix();

    gsd_popmatrix();

    return;
}

void gsd_draw_gyro(float *center, unsigned long colr, float siz)
{
    int i;

    gsd_pushmatrix();
    gsd_translate(center[X], center[Y], center[Z]);
    gsd_scale(siz, siz, siz);
    gsd_color_func(colr);

    /* vert axis */
    gsd_bgnline();
    gsd_vert_func(Octo[2]);
    gsd_vert_func(Octo[5]);
    gsd_endline();
   
    /* spokes */
    gsd_pushmatrix();
    
    for (i=0; i<6; i++)
    {
	gsd_rot(30., 'z');
	gsd_bgnline();
	gsd_vert_func(Octo[0]);
	gsd_vert_func(Octo[3]);
	gsd_endline();
    }
    
    gsd_popmatrix();

    gsd_color_func(colr);

    gsd_circ(0.,0.,1.);

    gsd_pushmatrix();
    gsd_rot(90., 'x');
    gsd_circ(0.,0.,1.);
    gsd_popmatrix();

    gsd_pushmatrix();
    gsd_rot(90., 'y');
    gsd_circ(0.,0.,1.);
    gsd_popmatrix();

    gsd_popmatrix();

    return;
}

void gsd_3dcursor(float *pt)
{
    float big, vert[3];

    big = 10000.;

    gsd_bgnline();
    vert[X] = pt[X];
    vert[Y] = pt[Y];
    vert[Z] = big;
    gsd_vert_func(vert);
    vert[Z] = -big;
    gsd_vert_func(vert);
    gsd_endline();

    gsd_bgnline();
    vert[X] = pt[X];
    vert[Z] = pt[Z];
    vert[Y] = big;
    gsd_vert_func(vert);
    vert[Y] = -big;
    gsd_vert_func(vert);
    gsd_endline();

    gsd_bgnline();
    vert[Y] = pt[Y];
    vert[Z] = pt[Z];
    vert[X] = big;
    gsd_vert_func(vert);
    vert[X] = -big;
    gsd_vert_func(vert);
    gsd_endline();

    return;
}

void dir_to_slope_aspect(float *dir, float *slope, float *aspect, int degrees)
{
    float dx, dy, dz;
    float costheta, theta, adjacent;

    dx = dir[X];
    dy = dir[Y];
    dz = dir[Z];

    /* project vector <dx,dy,dz> onto plane of constant z containing
    * final value should be 0.0 to 3600.0 */
    if (dx == 0 && dy == 0)
    {
    	*aspect = 0.;
    }
    else
    { 
	if (dx == 0)
	{
	    theta = 90.0;
	}
	else
	{
	    costheta = dx / sqrt(dx*dx + dy*dy);
	    theta = acos(costheta);
	}
	
	if (dy < 0)
	{
	    theta = (2*Pi) - theta;
	}

	*aspect = theta;
    }

    /* project vector <dx,dy,dz> onto plane of constant y containing
    * final value should be -900.0 (looking up) to 900.0 (looking down)*/
    if (dz == 0)
    {
	theta = 0.0;
    }
    else if ( dx == 0 && dy == 0 )
    {
	theta = Pi/2.;
    }
    else
    {
	adjacent = sqrt(dx*dx + dy*dy);
	costheta = adjacent / sqrt(adjacent*adjacent + dz*dz);
	theta =  acos(costheta);
    }
    
    if (dz > 0)
    {
	theta =  -theta;
    }
    
    *slope = theta;

    if (degrees)
    {
      *aspect = *aspect * (180./Pi);
      *slope = *slope * (180./Pi);
    }

    return;
}

/**************************************************************/
/* siz is height, sz is global exag to correct for.
 * If onsurf in non-null, z component of dir is dropped and
 * line-on-suf is used, resulting in length of arrow being proportional
 * to slope 
*/
int gsd_arrow(float *center, unsigned long colr, float siz, float *dir,
    float sz, geosurf *onsurf)
{
    int i;
    float slope,aspect;
    float tmp[3];
    static int first=1;

    if (first)
    {
	init_stuff();
	first = 0;
    }

    dir[Z]/=sz;

    GS_v3norm(dir);

    if (NULL != onsurf)
    {
	float base[3], tip[3], len;

	base[X] = center[X];
	base[Y] = center[Y];
	
	/* project dir to surface, after zexag */
	len = GS_P2distance (ORIGIN, dir); /* in case dir isn't normalized */
	tip[X] = center[X]+dir[X]*len*siz;
	tip[Y] = center[Y]+dir[Y]*len*siz;
	
	return gsd_arrow_onsurf(base, tip, colr, 2, onsurf);
    }

    dir_to_slope_aspect(dir, &slope, &aspect, 1);

    gsd_pushmatrix();
    gsd_translate(center[X], center[Y], center[Z]);
    gsd_scale(1.0, 1.0, 1.0/sz);
    gsd_rot(aspect+90, 'z');
    gsd_rot(slope+90., 'x');
    gsd_scale(siz, siz, siz);
    gsd_color_func(colr);

    tmp[X]=0.2;
    tmp[Y]=0.0;
    tmp[Z]=0.65;
   
    gsd_bgnline();
	gsd_vert_func(ORIGIN);
	gsd_vert_func(UP_NORM);
    gsd_endline();
    
    gsd_bgnline();
	gsd_vert_func(tmp);
	gsd_vert_func(UP_NORM);
	tmp[X]=-0.2;
	gsd_vert_func(tmp);
    gsd_endline();

    gsd_popmatrix();
    
    return(1);
}

/**************************************************************/
int gsd_arrow_onsurf(float *base, float *tip, unsigned long colr, int wid,
    geosurf *gs)
{
    int i;
    float tmp[3], dir2[3], dir1[3], dir0[3], len, dim1, dim2;
    static int first=1;

    if (first)
    {
	init_stuff();
	first = 0;
    }
    
    gsd_linewidth(wid);
    gsd_color_func(colr);

    /* DEBUG
    fprintf(stderr,"%f %f -> ", base[X], base[Y]);
    fprintf(stderr,"%f %f\n", tip[X], tip[Y]);
    */
    gsd_line_onsurf(gs, base, tip);

    #ifdef DO_SPHERE_BASE
    {
	GS_v3dir(tip, base, dir0);
	GS_v3mag(dir0, &len);
	gsd_disc(base[X], base[Y], len/10.);
    }
    #endif

    #ifdef ARROW_READY
    {
	base[Z] = tip[Z] = 0.0;
	GS_v3dir(tip, base, dir0);

	/* DEBUG
	fprintf(stderr,"dir0: %f %f %f\n", dir0[X], dir0[Y], dir0[Z]);
	*/

	/* rotate this direction 90 degrees */
	GS_v3cross(dir0, UP_NORM, dir2);
	GS_v3mag(dir0, &len);
	GS_v3eq(dir1, dir0);

	/* DEBUG
	fprintf(stderr,"len: %f\n", len);
	fprintf(stderr,"a-dir1: %f %f %f\n", dir1[X], dir1[Y], dir1[Z]);
	fprintf(stderr,"a-dir2: %f %f %f\n", dir2[X], dir2[Y], dir2[Z]);
	*/
	dim1=len*.7;
	dim2=len*.2;
	GS_v3mult(dir1,dim1);
	GS_v3mult(dir2,dim2);

	/* DEBUG
	fprintf(stderr,"b-dir1: %f %f %f\n", dir1[X], dir1[Y], dir1[Z]);
	fprintf(stderr,"b-dir2: %f %f %f\n", dir2[X], dir2[Y], dir2[Z]);
	*/
	GS_v3eq(tmp, base);
	GS_v3add(tmp, dir1);
	GS_v3add(tmp, dir2);

	/* DEBUG
	fprintf(stderr,"%f %f -> ", tmp[X], tmp[Y]);
	*/
	gsd_line_onsurf(gs, tmp, tip);

	GS_v3cross(dir0, DOWN_NORM, dir2);
	GS_v3mult(dir2,dim2);
	GS_v3eq(tmp, base);

	/* DEBUG
	fprintf(stderr,"dir1: %f %f %f\n", dir1[X], dir1[Y], dir1[Z]);
	fprintf(stderr,"dir2: %f %f %f\n", dir2[X], dir2[Y], dir2[Z]);
	*/
	GS_v3add(tmp, dir1);
	GS_v3add(tmp, dir2);

	/* DEBUG
	fprintf(stderr,"%f %f\n", tmp[X], tmp[Y]);
	*/
	gsd_line_onsurf(gs, tip, tmp);
    }
    #endif

    return(0);
}


/**************************************************************/
/* siz1 is height, siz2 is diameter */
void gsd_3darrow(float *center, unsigned long colr, float siz1, float siz2,
    float *dir, float sz)
{
    int i;
    float slope,aspect;
    int preshade;
    static int first=1;
    static int list;

    dir[Z]/=sz;

    GS_v3norm(dir);
    dir_to_slope_aspect(dir, &slope, &aspect, 1);

    #ifdef DEBUG
    {
	static int debugint=1;

	if (debugint>100)
	{
    	    fprintf(stderr,"pt: %f,%f,%f dir: %f,%f,%f slope: %f aspect: %f\n", 
    	    center[X],center[Y],center[Z],dir[X],dir[Y],dir[Z],slope,aspect);
    	    debugint=1;
	}
	debugint++;
    }
    #endif

    preshade = gsd_getshademodel();

    /* 
    gsd_shademodel(0);  
    want flat shading? */
    gsd_pushmatrix();
    gsd_translate(center[X], center[Y], center[Z]);
    gsd_scale(1.0, 1.0, 1.0/sz);
    gsd_rot(aspect+90, 'z');
    gsd_rot(slope+90., 'x');
    gsd_scale(siz2, siz2, siz1);
    gsd_color_func(colr);

    if (first)
    {
    	/* combine these into an object */
	first = 0;
	list = gsd_makelist();
	gsd_bgnlist(list, 1);
	gsd_backface(1);

	gsd_pushmatrix();
	gsd_scale(.10, .10, .75); /* narrow cyl */
	primitive_cylinder(colr, 0);
	gsd_popmatrix();

	gsd_pushmatrix();
	gsd_translate(0.0, 0.0, .60);
	gsd_scale(0.3, 0.3, 0.4); /* cone */
	primitive_cone(colr);
	gsd_popmatrix();

	gsd_backface(0);
	gsd_endlist();
    }
    else
    {
	gsd_calllist(list);
    }

    gsd_popmatrix();
    gsd_shademodel(preshade);

    return;
}

/* primitives only called after transforms */
/* center is actually center at base of 8 sided cone */
void primitive_cone(unsigned long col)
{
    float tip[3];
    static int first=1;

    if (first)
    {
	init_stuff();
	first = 0;
    }

    tip[X] = tip[Y] = 0.0;
    tip[Z] = 1.0;

    gsd_bgntfan();
    gsd_litvert_func2(UP_NORM, col, tip);
    gsd_litvert_func2(ogverts[0], col, ogverts[0]);
    gsd_litvert_func2(ogverts[1], col, ogverts[1]);
    gsd_litvert_func2(ogverts[2], col, ogverts[2]);
    gsd_litvert_func2(ogverts[3], col, ogverts[3]);
    gsd_litvert_func2(ogverts[4], col, ogverts[4]);
    gsd_litvert_func2(ogverts[5], col, ogverts[5]);
    gsd_litvert_func2(ogverts[6], col, ogverts[6]);
    gsd_litvert_func2(ogverts[7], col, ogverts[7]);
    gsd_litvert_func2(ogverts[0], col, ogverts[0]);
    gsd_endtfan();
    
    return;
}

/* primitives only called after transforms */
/* center is actually center at base of 8 sided cylinder */
void primitive_cylinder(unsigned long col, int caps)
{
    static int first=1;

    if (first)
    {
	init_stuff();
	first = 0;
    }

    gsd_bgnqstrip();
    gsd_litvert_func2(ogverts[0], col, ogvertsplus[0]);
    gsd_litvert_func2(ogverts[0], col, ogverts[0]);
    gsd_litvert_func2(ogverts[1], col, ogvertsplus[1]);
    gsd_litvert_func2(ogverts[1], col, ogverts[1]);
    gsd_litvert_func2(ogverts[2], col, ogvertsplus[2]);
    gsd_litvert_func2(ogverts[2], col, ogverts[2]);
    gsd_litvert_func2(ogverts[3], col, ogvertsplus[3]);
    gsd_litvert_func2(ogverts[3], col, ogverts[3]);
    gsd_litvert_func2(ogverts[4], col, ogvertsplus[4]);
    gsd_litvert_func2(ogverts[4], col, ogverts[4]);
    gsd_litvert_func2(ogverts[5], col, ogvertsplus[5]);
    gsd_litvert_func2(ogverts[5], col, ogverts[5]);
    gsd_litvert_func2(ogverts[6], col, ogvertsplus[6]);
    gsd_litvert_func2(ogverts[6], col, ogverts[6]);
    gsd_litvert_func2(ogverts[7], col, ogvertsplus[7]);
    gsd_litvert_func2(ogverts[7], col, ogverts[7]);
    gsd_litvert_func2(ogverts[0], col, ogvertsplus[0]);
    gsd_litvert_func2(ogverts[0], col, ogverts[0]);
    gsd_endqstrip();

    if (caps)
    {
	/* draw top */
	gsd_bgntfan();
	gsd_litvert_func2(UP_NORM, col, UP_NORM);
	gsd_litvert_func2(UP_NORM, col, ogvertsplus[0]);
	gsd_litvert_func2(UP_NORM, col, ogvertsplus[1]);
	gsd_litvert_func2(UP_NORM, col, ogvertsplus[2]);
	gsd_litvert_func2(UP_NORM, col, ogvertsplus[3]);
	gsd_litvert_func2(UP_NORM, col, ogvertsplus[4]);
	gsd_litvert_func2(UP_NORM, col, ogvertsplus[5]);
	gsd_litvert_func2(UP_NORM, col, ogvertsplus[6]);
	gsd_litvert_func2(UP_NORM, col, ogvertsplus[7]);
	gsd_litvert_func2(UP_NORM, col, ogvertsplus[0]);
	gsd_endtfan();

	/* draw bottom */
	gsd_bgntfan();
	gsd_litvert_func2(DOWN_NORM, col, ORIGIN);
	gsd_litvert_func2(DOWN_NORM, col, ogverts[0]);
	gsd_litvert_func2(DOWN_NORM, col, ogverts[1]);
	gsd_litvert_func2(DOWN_NORM, col, ogverts[2]);
	gsd_litvert_func2(DOWN_NORM, col, ogverts[3]);
	gsd_litvert_func2(DOWN_NORM, col, ogverts[4]);
	gsd_litvert_func2(DOWN_NORM, col, ogverts[5]);
	gsd_litvert_func2(DOWN_NORM, col, ogverts[6]);
	gsd_litvert_func2(DOWN_NORM, col, ogverts[7]);
	gsd_litvert_func2(DOWN_NORM, col, ogverts[0]);
	gsd_endtfan();
    }

    return;
}
