
/*  gsd.c
    Bill Brown, USACERL  
    October 1993
*/
	
#include "gstypes.h"
#include "gsget.h"
#include "math.h"
#include <stdio.h> 
/* #include <stdlib.h>
*/
#include "rowcol.h"

/*
#define TRACE_DFUNCS
*/

Point3 *gsdrape_get_segments();

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

gsd_plus(center, colr, siz)
float center[3], siz;
int colr;
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


}

gsd_line_onsurf(gs, v1, v2)
geosurf *gs;
float *v1, *v2;
{
int i, np;
Point3 *pts;
float fudge;

    pts = gsdrape_get_segments(gs, v1, v2, &np);
    if(pts){
	fudge = FUDGE(gs);
	gsd_bgnline();
	for (i=0; i<np; i++){
	    pts[i][Z] += fudge;
	    gsd_vert_func(pts[i]);
	}
	gsd_endline();
    }

}

gsd_x(gs, center, colr, siz)
geosurf *gs;   /* NULL if flat */
float center[3], siz;
int colr;
{
float v1[3], v2[3];

    gsd_color_func(colr);
    siz *= .5;
   
    v1[Z] = v2[Z] = center[Z];

    v1[X] = center[X] - siz;
    v2[X] = center[X] + siz;
    v1[Y] = center[Y] - siz;
    v2[Y] = center[Y] + siz;

    if(gs){
	gsd_line_onsurf(gs, v1, v2);
    }
    else{
	gsd_bgnline();
	    gsd_vert_func(v1);
	    gsd_vert_func(v2);
	gsd_endline();
    }

    v1[X] = center[X] - siz;
    v2[X] = center[X] + siz;
    v1[Y] = center[Y] + siz;
    v2[Y] = center[Y] - siz;

    if(gs){
	gsd_line_onsurf(gs, v1, v2);
    }
    else{
	gsd_bgnline();
	    gsd_vert_func(v1);
	    gsd_vert_func(v2);
	gsd_endline();
    }

}

gsd_diamond(center, colr, siz)
float center[3], siz;
int colr;
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
#endif

    gsd_popmatrix();
    gsd_shademodel(preshade);

}

gsd_diamond_lines()
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

}

gsd_drawsphere(center, colr, siz)
float center[3], siz;
unsigned long colr;
{
    siz *= .5;  /* siz is diameter, gsd_sphere uses radius */
    gsd_color_func(colr);
    gsd_sphere(center, siz);

}

gsd_draw_asterisk(center, colr, siz)
float center[3], siz;
unsigned long colr;
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

}


gsd_draw_gyro(center, colr, siz)
float center[3], siz;
unsigned long colr;
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
    for (i=0; i<6; i++){
	gsd_rot(30., 'z');
	gsd_bgnline();
	gsd_vert_func(Octo[0]);
	gsd_vert_func(Octo[3]);
	gsd_endline();
    }
    gsd_popmatrix();

    gsd_color_func(~colr);

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

}

gsd_3dcursor(pt)
float pt[3];
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

}


