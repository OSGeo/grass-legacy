
/*  gsd_cplane.c
    Bill Brown, USACERL  
    January 1993
*/

#include "gstypes.h"

/* DEBUG */
#include <stdio.h>
#include "rowcol.h"

extern float GS_P2distance ();

static float Cp_pt[4], Cp_norm[MAX_CPLANES][4];
static float Cp_trans[MAX_CPLANES][3], Cp_rot[MAX_CPLANES][3];
static int Cp_ison[MAX_CPLANES]; /* also need isdef? */

static int
init_cplane()
{
int i;
    
    gs_get_datacenter(Cp_pt);
    gs_get_data_avg_zmax(&(Cp_pt[Z]));
    for (i=0; i< MAX_CPLANES; i++){
	Cp_ison[i] = 0;
	Cp_norm[i][X] = 1.0;
	Cp_norm[i][Y] = Cp_norm[i][Z] = 0.0;
	Cp_norm[i][W] = 1.;
	Cp_rot[i][X] = Cp_trans[i][X] = 0.0;
	Cp_rot[i][Y] = Cp_trans[i][Y] = 0.0;
	Cp_rot[i][Z] = Cp_trans[i][Z] = 0.0;
    }
}

/************************************************************************/
gsd_def_cplane(num, pt, norm)
int num;
float pt[3], norm[3];
{
float sx, sy, sz, ppt[3]; 
double params[4];
float zmin, zmax;

    GS_get_scale(&sx, &sy, &sz, 1);

    /* Something's still wrong with the zexag - DONT USE TILT */
    GS_get_zrange(&zmin, &zmax, 0);

    ppt[0] = (pt[0]+Cp_pt[0])*sx;
    ppt[1] = (pt[1]+Cp_pt[1])*sy;
    ppt[2] = (pt[2]+Cp_pt[2]-zmin)*sz;

    params[0] = norm[0]*sx;
    params[1] = norm[1]*sy;
    params[2] = norm[2]*sz;
    GS_dv3norm(params);
    params[3] = -ppt[0]*params[0] - ppt[1]*params[1] - 
		ppt[2]*params[2];

    gsd_def_clipplane(num, params);

}

/************************************************************************/
/* called when viewing matrix changes */
gsd_update_cplanes()
{
int i;

    for(i=0; i<MAX_CPLANES; i++){
	if(Cp_ison[i])
	    gsd_def_cplane(i, Cp_trans[i], Cp_norm[i]);
    }

}

/************************************************************************/
gsd_cplane_on(num)
int num;
{
static int first=1;
float params[4]; /* ignored? */

    if(first){
	first=0;
	init_cplane();
	gsd_def_cplane(num, Cp_trans[num], Cp_norm[num]);
    }
    
    gsd_set_clipplane(num, 1);

    Cp_ison[num] = 1;
}
/************************************************************************/
gsd_cplane_off(num)
int num;
{
float params[4]; /* ignored? */

    gsd_set_clipplane(num, 0);
    Cp_ison[num] = 0;
}

/************************************************************************/
/* onstate MUST be big enough to hold MAX_CPLANES ints */
gsd_get_cplanes_state(onstate)
int *onstate;
{
int i;

    for(i=0; i< MAX_CPLANES; i++){
	onstate[i] = Cp_ison[i];
    }

}


/************************************************************************/
/* planes MUST be big enough to hold MAX_CPLANES Point4s */
/* returns surface coordinates, normal pointing away from visible side */
gsd_get_cplanes(planes)
Point4 planes[];
{
int i, ons;
Point3 thru;
    
    for(ons=i=0; i< MAX_CPLANES; i++){
	if(Cp_ison[i]){
	    thru[X] = Cp_pt[X] + Cp_trans[ons][X];
	    thru[Y] = Cp_pt[Y] + Cp_trans[ons][Y];
	    thru[Z] = Cp_pt[Z] + Cp_trans[ons][Z];
	    planes[ons][X] = -Cp_norm[ons][X];
	    planes[ons][Y] = -Cp_norm[ons][Y];
	    planes[ons][Z] = -Cp_norm[ons][Z];
	    planes[ons][W] = -(DOT3(planes[ons],thru));
	    ons++;
	}
    }
    return(ons);

}

/************************************************************************/
gsd_update_cpnorm(num)
int num;
{
float v[4];

    v[X] = v[W] = 1.0;
    v[Y] = v[Z] = 0.0;

    P_pushmatrix();
    P_rot(Cp_rot[num][Z], 'z');
    P_rot(Cp_rot[num][Y], 'y');
    P_rot(Cp_rot[num][X], 'x');
    P_transform (1, v, Cp_norm[num]);
    P_popmatrix();

}

/************************************************************************/
gsd_cplane_setrot(num, rx, ry, rz)
int num;
float rx, ry, rz;
{

    Cp_rot[num][X] = rx; 
    Cp_rot[num][Y] = ry; 
    Cp_rot[num][Z] = rz; 
    
    gsd_update_cpnorm(num);
    gsd_def_cplane(num, Cp_trans[num], Cp_norm[num]);
}
/************************************************************************/
gsd_cplane_settrans(num, tx, ty, tz)
int num;
float tx, ty, tz;
{

    
    Cp_trans[num][X] = tx;
    Cp_trans[num][Y] = ty;
    Cp_trans[num][Z] = tz;

    gsd_def_cplane(num, Cp_trans[num], Cp_norm[num]);

}

/************************************************************************/
gsd_draw_cplane_fence(surf1, surf2, cpnum)
geosurf *surf1, *surf2;
int cpnum;
{
int was_on;
float params[4]; /* not used */
float len, dir[3], bgn[2], end[2], px, py, fencenorm[3];

    /* temporarily turn this plane off */
    if(was_on = Cp_ison[cpnum])
	gsd_set_clipplane(cpnum, 0);
    
    /* line on surface (asuming NO TILT) is (-A,B)->(A,-B), 
    extended thru Cp_pt */
    dir[X] = -Cp_norm[cpnum][Y];  
    dir[Y] = Cp_norm[cpnum][X];
    dir[Z] = 0.0;
    GS_v3norm(dir);
    px = Cp_trans[cpnum][X]+Cp_pt[X];
    py = Cp_trans[cpnum][Y]+Cp_pt[Y];
    /* TODO: make line from point & direction, clip to region */
    /* for now, to test: */
    bgn[X] = px;
    bgn[Y] = py;
    end[X] = px;
    end[Y] = 3 * VROW2Y(surf1,0);
    len = GS_P2distance (bgn, end) - 1;
    bgn[X] = px + len * dir[X];
    bgn[Y] = py + len * dir[Y];
    end[X] = px - len * dir[X];
    end[Y] = py - len * dir[Y];
/*
fprintf(stderr,"plane centered on %f,%f len=%f\n", px, py, len);
fprintf(stderr,"Cp_pt = %f,%f,%f\n",Cp_pt[X], Cp_pt[Y],Cp_pt[Z]);
fprintf(stderr,"plane intersects along %f,%f %f,%f\n", bgn[X], bgn[Y],
 end[X], end[Y]);
*/
    fencenorm[X] = -Cp_norm[cpnum][X];
    fencenorm[Y] = -Cp_norm[cpnum][Y];
    fencenorm[Z] = -Cp_norm[cpnum][Z];
    gsd_wall(bgn, end, fencenorm);
	
    /* turn this plane back on */
    if(was_on)
	gsd_set_clipplane(cpnum, 1);

}

/************************************************************************/
gsd_draw_cplane(num)
int num;
{
float size, cpv[3];
int i;
float params[4], scalez;
unsigned long colr;


    /* turn off all clipping planes */
    for(i=0; i<MAX_CPLANES; i++){
	if(Cp_ison[i])
	    gsd_set_clipplane(i, 0);
    }


    GS_get_longdim(&size);
    size /= 2.;
    cpv[X] = 0.0;
    
    gsd_blend(1);

    gsd_zwritemask(0x0);

    gsd_pushmatrix();

    gsd_do_scale(1);

    gsd_translate(Cp_pt[X] + Cp_trans[num][X], 
		  Cp_pt[Y] + Cp_trans[num][Y], 
		  Cp_pt[Z] + Cp_trans[num][Z]);

    gsd_rot(Cp_rot[num][Z], 'z');
    gsd_rot(Cp_rot[num][Y], 'y');
    gsd_rot(Cp_rot[num][X], 'x');

    if(scalez = GS_global_exag())
	gsd_scale(1.0, 1.0, 1./scalez);

   
    colr = (GS_default_draw_color() | 0xff000000) & 0x33ffffff;
    gsd_color_func(colr);
    gsd_bgnpolygon();
    cpv[Y] = size;
    cpv[Z] = size;
    gsd_vert_func(cpv);
    cpv[Y] = -size;
    gsd_vert_func(cpv);
    cpv[Z] = -size;
    gsd_vert_func(cpv);
    cpv[Y] = size;
    gsd_vert_func(cpv);
    gsd_endpolygon();


    gsd_popmatrix();
    gsd_blend(0);
    gsd_zwritemask(0xffffffff);

    /* turn on clipping planes */
    for(i=0; i<MAX_CPLANES; i++){
	if(Cp_ison[i])
	    gsd_set_clipplane(i, 1);
    }

}
/************************************************************************/


