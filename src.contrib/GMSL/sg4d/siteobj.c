/*
**  Written by Bill Brown, Winter 1991 - 1992
**  US Army Construction Engineering Research Lab
*/

/*
** Copyright USA CERL 1992. All rights reserved.
*/

/* NOT ALL site objects work with Latlon on globe */

#include "externs.h"
#include <math.h>

#ifdef DEBUG
/********************************************/
/********************************************/
/* timing stuff for optimization debugging  */

#include <time.h>

time_t first_time;

bgntime()
{
    first_time = time(NULL);
}

endtime()
{
    fprintf(stderr,"Elapsed time = %ld seconds.\n", time(NULL) - first_time);
}
    
/********************************************/
/********************************************/
#endif


/* normals for octohedron */
float octnorms[6][3] = {
    { 1.0,  0.0,  0.0},
    { 0.0,  1.0,  0.0},
    { 0.0,  0.0,  1.0},
    {-1.0,  0.0,  0.0},
    { 0.0, -1.0,  0.0},
    { 0.0,  0.0, -1.0}
};


#define UP_NORM octnorms[2]


/* vertices for octagon in xy plane */
float ogverts[8][3];

/* vertices for octagon in xy plane, z=4 */
float ogvertsplus[8][3];


void vertex(i)
int i;
{
    n3f(octnorms[i]);
    v3f(octnorms[i]);
}

init_ogverts()
{
float cos45;
int i;

    cos45 = cos(PI/4.0);

    for(i=0; i<8; i++){
	ogverts[i][Z] = 0.0;
	ogvertsplus[i][Z] = 4.0;
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


}


/* center is actually center at base of 8 sided cone */
draw_cone(center, col, siz) 
float center[3], siz;
unsigned long col;
{
float tip[3];
static int first=1;

    if(first){
	init_ogverts();
	first = 0;
    }

    siz *= .5;
    cpack(col);

    pushmatrix();
    translate(center[X], center[Y], center[Z]);
    scale(siz, siz, siz); 

    tip[X] = tip[Y] = 0.0;
    tip[Z] = 2.0; /* height to width ratio = 2 */

    bgntmesh();
    n3f(ogverts[0]);
    v3f(ogverts[0]);
    n3f(ogverts[1]);
    v3f(ogverts[1]);
    n3f(UP_NORM);
    v3f(tip);
    n3f(ogverts[2]);
    v3f(ogverts[2]);
    swaptmesh();
    n3f(ogverts[3]);
    v3f(ogverts[3]);
    swaptmesh();
    n3f(ogverts[4]);
    v3f(ogverts[4]);
    swaptmesh();
    n3f(ogverts[5]);
    v3f(ogverts[5]);
    swaptmesh();
    n3f(ogverts[6]);
    v3f(ogverts[6]);
    swaptmesh();
    n3f(ogverts[7]);
    v3f(ogverts[7]);
    swaptmesh();
    n3f(ogverts[0]);
    v3f(ogverts[0]);
    endtmesh();
    
    popmatrix();
}


draw_conetree(center, siz)
float center[3], siz;
{
static int first=1;
float newcenter[3];

    if(first){
	init_ogverts();
	first = 0;
    }

    draw_treetrunk(center, siz);
    newcenter[X] = center[X];
    newcenter[Y] = center[Y];
    newcenter[Z] = center[Z] + .3 * siz; /*trunk height = .5 * siz. (.125 * 4)*/
    draw_cone(newcenter, 0x307040, siz); /* bluish dark green */

}


draw_roundtree(center, siz)
float center[3], siz;
{
static int first=1;
float newcenter[3];

    if(first){
	init_ogverts();
	first = 0;
    }

    draw_treetrunk(center, siz);
    newcenter[X] = center[X];
    newcenter[Y] = center[Y];
    newcenter[Z] = center[Z] + .75 * siz; /*trunk height = .5 * siz. (.125 * 4)*/
    draw_sphere(newcenter, 0x10A040, siz*.75, 1.25); /* tree green */

}



draw_treetrunk(center, siz)
float center[3], siz;
{
static int first=1;

    if(first){
	init_ogverts();
	first = 0;
    }

    siz *= .125;
    cpack(0x002850); /* brown */

    pushmatrix();
    translate(center[X], center[Y], center[Z]);
    scale(siz, siz, siz);

    bgnqstrip();
    n3f(ogverts[0]);
    v3f(ogvertsplus[0]);
    v3f(ogverts[0]);
    n3f(ogverts[1]);
    v3f(ogvertsplus[1]);
    v3f(ogverts[1]);
    n3f(ogverts[2]);
    v3f(ogvertsplus[2]);
    v3f(ogverts[2]);
    n3f(ogverts[3]);
    v3f(ogvertsplus[3]);
    v3f(ogverts[3]);
    n3f(ogverts[4]);
    v3f(ogvertsplus[4]);
    v3f(ogverts[4]);
    n3f(ogverts[5]);
    v3f(ogvertsplus[5]);
    v3f(ogverts[5]);
    n3f(ogverts[6]);
    v3f(ogvertsplus[6]);
    v3f(ogverts[6]);
    n3f(ogverts[7]);
    v3f(ogvertsplus[7]);
    v3f(ogverts[7]);
    n3f(ogverts[0]);
    v3f(ogvertsplus[0]);
    v3f(ogverts[0]);
    endqstrip();


    popmatrix();

}



draw_octo(center, col, siz)
float center[3], siz;
unsigned long col;
{

    if(LatLon){
	center[X] = (center[X])/XYscale+wind.west;
	center[Y] = (center[Y])/XYscale+wind.south;
	latlon_to_sphere(center, RADIUS);
    }

    siz *= .5;
    cpack(col);

    pushmatrix();
    translate(center[X], center[Y], center[Z]);
    scale(siz, siz, siz);

    bgntmesh();
	vertex(0);
	vertex(1);
    swaptmesh();
	vertex(2);
    swaptmesh();
	vertex(4);
    swaptmesh();
    	vertex(5);
    swaptmesh();
	vertex(1);
	vertex(3);
	vertex(2);
    swaptmesh();
	vertex(4);
    swaptmesh();
	vertex(5);
    swaptmesh();
	vertex(1);
    endtmesh();

    popmatrix();
}

/*
#define TRY_SPHERELIB
*/

#ifdef TRY_SPHERELIB
#include <gl/sphere.h>
#endif

draw_sphere(center, col, siz, warp)
float center[3], siz, warp;
unsigned long col;
{

    if(LatLon){
	center[X] = (center[X])/XYscale+wind.west;
	center[Y] = (center[Y])/XYscale+wind.south;
	latlon_to_sphere(center, RADIUS);
    }

#ifdef TRY_SPHERELIB
{
float params[4];
static int first=1;

    if(first){
	/* set sphere mode */
	sphmode(SPH_ORIENT, FALSE);
	first = 0;
    }

    siz *= .5;
    cpack(col);
    
    pushmatrix();
    scale(1.0, 1.0, warp);
    params[0]=center[0];
    params[1]=center[1];
    params[2]=center[2];
    params[3]=siz;
    sphdraw(params);
    popmatrix();
}
#else
{
int i, j, arc_offset;
float *n;


    siz *= .5;
    cpack(col);

    pushmatrix();
    translate(center[X], center[Y], center[Z]);
    scale(siz, siz, siz*warp);
    arc_offset =  SP_HRESP * 3;
    for(i=0; i<(SP_RES-1); i++){
	for(j=0; j<SP_HRES; j++){
	    bgnpolygon();
	    if(j != 0){
		n = &normp[(i+1) * arc_offset + j*3];
		n3f(n);
		v3f(n);
	    }
	    n = &normp[i * arc_offset + j*3];
	    n3f(n);
	    v3f(n);
	    n = &n[3];
	    n3f(n);
	    v3f(n);
	    if(j != (SP_HRES-1)){
		n = &normp[(i+1) * arc_offset + (j+1)*3];
		n3f(n);
		v3f(n);
	    }
	    endpolygon();
	}
    }

    for(j=0; j<SP_HRES; j++){  
	    bgnpolygon();
	    if(j != 0){
		n = &normp[j*3];
		n3f(n);
		v3f(n);
	    }
	    n = &normp[i * arc_offset + j*3];
	    n3f(n);
	    v3f(n);
	    n = &n[3];
	    n3f(n);
	    v3f(n);
	    if(j != (SP_HRES-1)){
		n = &normp[(j+1)*3];
		n3f(n);
		v3f(n);
	    }
	    endpolygon();
    }

    popmatrix();
}

#endif /* TRY_SPHERELIB */


}


draw_litsphere(center, siz)
float center[3], siz;
{

#ifdef TRY_SPHERELIB
float params[4];
static int first=1;

    if(first){
	/* set sphere mode */
	sphmode(SPH_ORIENT, FALSE);
	first = 0;
    }

    pushmatrix();
    params[0]=center[0];
    params[1]=center[1];
    params[2]=center[2];
    params[3]=siz;
    sphdraw(params);
    popmatrix();
#else

int i, j, arc_offset;
float *n;


    pushmatrix();
    translate(center[X], center[Y], center[Z]);
    scale(siz, siz, siz);
    arc_offset =  SP_HRESP * 3;
    for(i=0; i<(SP_RES-1); i++){
	for(j=0; j<SP_HRES; j++){
	    bgnpolygon();
	    if(j != 0){
		n = &normp[(i+1) * arc_offset + j*3];
		n3f(n);
		v3f(n);
	    }
	    n = &normp[i * arc_offset + j*3];
	    n3f(n);
	    v3f(n);
	    n = &n[3];
	    n3f(n);
	    v3f(n);
	    if(j != (SP_HRES-1)){
		n = &normp[(i+1) * arc_offset + (j+1)*3];
		n3f(n);
		v3f(n);
	    }
	    endpolygon();
	}
    }
    for(j=0; j<SP_HRES; j++){  
	    bgnpolygon();
	    if(j != 0){
		n = &normp[j*3];
		n3f(n);
		v3f(n);
	    }
	    n = &normp[i * arc_offset + j*3];
	    n3f(n);
	    v3f(n);
	    n = &n[3];
	    n3f(n);
	    v3f(n);
	    if(j != (SP_HRES-1)){
		n = &normp[(j+1)*3];
		n3f(n);
		v3f(n);
	    }
	    endpolygon();
    }

    popmatrix();

#endif /* TRY_SPHERELIB */


}


draw_dome(center, col, siz, warp, dir)
float center[3], siz, warp;
unsigned long col;
int dir;
{
int i, j, arc_offset;
float *n;

    if(LatLon){
	center[X] = (center[X])/XYscale+wind.west;
	center[Y] = (center[Y])/XYscale+wind.south;
	latlon_to_sphere(center, RADIUS);
    }

    siz *= .5;
    cpack(col);

    pushmatrix();
    translate(center[X], center[Y], center[Z]);
    scale(siz, siz, siz*warp);

    /* rotate according to dir - order may be important here (scale, rot) */
    if(dir)
	rotate(1800, 'y');

    arc_offset =  SP_HRESP * 3;
    for(i=0; i<(SP_HRES); i++){
	for(j=0; j<SP_HRES; j++){
	    bgnpolygon();
	    if(j != 0){
		n = &normp[(i+1) * arc_offset + j*3];
		n3f(n);
		v3f(n);
	    }
	    n = &normp[i * arc_offset + j*3];
	    n3f(n);
	    v3f(n);
	    n = &n[3];
	    n3f(n);
	    v3f(n);
	    if(j != (SP_HRES-1)){
		n = &normp[(i+1) * arc_offset + (j+1)*3];
		n3f(n);
		v3f(n);
	    }
	    endpolygon();
	}
    }

    popmatrix();

}



