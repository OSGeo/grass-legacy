
/*
**  Written by Bill Brown, Winter 1991 - 1992 
**  US Army Construction Engineering Research Lab
*/

/*
** Copyright USA CERL 1992. All rights reserved.
*/


#include "externs.h"
/*
#include "gis.h"
*/
#include <stdio.h>
#include <math.h>

extern float Range_val[];
float world_to_real();
float real_to_world();
void _do_globe();


void
save_settings()

{
    FILE *fp;
    char *mapset;
    char name[30];
    struct Cell_head w;
    struct G_3dview v;

    
    w = wind;

    mapset = G_ask_new
	    ("Enter name to be used for saving current viewing options:",
	    name, "3d.view", "3d.view");

    if(mapset != NULL)
    {

    v.display_type = Agpoly->val? 3: ((int)Agrid->val | ((int)Apoly->val << 1)); 
    if(Aglobe->val){
	v.display_type += 10;   /* add 10 if mapped to globe */
	v.display_type += CenterSphere  * 10;   /* add 10 if CenterSphere ON */
    }
    v.from_to[TO][X] = FROM_TO[TO][X]/XYscale + w.west;
    v.from_to[TO][Y] = FROM_TO[TO][Y]/XYscale + w.south;
    if(Z_exag)
	v.from_to[TO][Z] = FROM_TO[TO][Z]/Z_exag + Z_Min_real;
    else
	v.from_to[TO][Z] = 0.0;
    
    v.from_to[FROM][X] = FROM_TO[FROM][X]/XYscale + w.west;
    v.from_to[FROM][Y] = FROM_TO[FROM][Y]/XYscale + w.south;
    if(Z_exag)
	v.from_to[FROM][Z] = FROM_TO[FROM][Z]/Z_exag + Z_Min_real;
    else
	v.from_to[FROM][Z] = 0.0;
    
    v.exag = Z_exag/XYscale;
    v.mesh_freq = fast_res;   /* mesh resolution */
    v.fov = Aortho->val? 0.0: Apersp->val / 10.0;
    v.poly_freq = slow_res;   /* poly resolution */
    v.dozero = (!(int)Anozero->val);

    v.lightson = (int)Alight->val; 
    v.twist = Atwist->val;
    v.colorgrid = (int)Agridc->val; /* 1 = use color */
    v.shading = (int)Ashading->val;
    v.fringe = (int)Afringe->val;
    v.lightpos[X] =
	(PNL_ACCESS (Point, Alightxy, x) * XRange + XBase)/XYscale + X_Min_real;
    v.lightpos[Y] =
	(PNL_ACCESS (Point, Alightxy, y) * YRange + YBase)/XYscale + Y_Min_real;
    v.lightpos[Z] =
	Z_exag?(Altheight->val * ZRange + ZBase)/Z_exag + Z_Min_real:0.0;
    v.lightpos[W] = 1.0;    /* local */
    v.lightcol[0] = Ared->val * Abright->val;
    v.lightcol[1] = Agrn->val * Abright->val;
    v.lightcol[2] = Ablu->val * Abright->val;
    v.ambient = Aambient->val;
    v.shine = Ashine->val;
    v.surfonly = (int)Asurface->val;
    strcpy((v.pgm_id), "SG3d");

    G_put_3dview(name, mapset, &v, &w);

    }
    else
	fprintf(stderr," ...current settings not saved\n");

}

/*
#define PR_NEW 1
*/

void
get_settings(vname)
char *vname;
{
    FILE *fp;
    char name[64];
    char *mapset;
    float view_dir, incl_dir, tempf;
    struct G_3dview v;
    struct Cell_head w;
    int tempi, ret = -1;

    w = wind;

    if(NULL == vname){
	mapset = G_ask_old
	    ("Enter name of previously saved viewing options:",
	    name, "3d.view", "3d.view");
    }
    else{
	strcpy (name, vname);
	mapset = G_find_file2 ("3d.view", name, "");
    }

    if(mapset != NULL)
	ret = G_get_3dview(name, mapset, &v);
    if(ret >= 0)
    {

	if(strcmp((v.pgm_id), "SG3d")){
	    fprintf(stderr,"WARNING: view not saved by this program,\n");
	    fprintf(stderr,"         there may be some inconsistancies.\n");
	}

	if(v.vwin.ns_res != w.ns_res){
	    v.mesh_freq = (int)(v.mesh_freq * v.vwin.ns_res/w.ns_res);
	    v.poly_freq = (int)(v.poly_freq * v.vwin.ns_res/w.ns_res);
	}

	Z_exag = v.exag;
	{
	    Aexag->val = Z_exag;
	    Z_exag *= XYscale;
	    Range = 7;   /* Range_val[Range] = 1.0 */
	    if(Aexag->val >= 1.0){
		while(Range_val[Range] < Aexag->val && Range >= 1)
		    Range--;
	    }
	    else{
		while((Range_val[Range] > Aexag->val) && Range<=13 ){
		    Range++;
		}
		Range--;
	    }
	    Aexag->maxval = 10 * Range_val[Range];
	    pnl_fixact (Aexag);
	}

	fast_res = v.mesh_freq;
	    update_fast_res ();
	slow_res = v.poly_freq;
	    update_slow_res ();

	Aglobe->val = 0;
	if(v.display_type >= 10){
	    v.display_type -= 10;
	    Aglobe->val = 1;    
	    if(v.display_type >= 10){
		v.display_type -= 10;
		CenterSphere = 1;    
	    }
	    else 
		CenterSphere = 0;
	}
	Agrid->val = Apoly->val = Agpoly->val = 0;
	switch (v.display_type){
	    case 1:
		Agrid->val = 1;
		Display_type = D_GRID;
		break;
	    case 2:
		Apoly->val = 1;
		Display_type = D_POLY;
		break;
	    case 3:
		Agpoly->val = 1;
		Display_type = D_GPOLY;
		break;
	}
	pnl_fixact (Agrid);
	pnl_fixact (Apoly);
	pnl_fixact(Agpoly);
	pnl_fixact(Aglobe);
	_do_globe();

	if(v.fov){
	    Apersp->val = v.fov * 10.0;
	    Aortho->val = 0;
	    _update_persp ();
	}
	else
	    Aortho->val = 1;
	pnl_fixact (Apersp);
	pnl_fixact (Aortho);

	Anozero->val = !v.dozero;
	    pnl_fixact (Anozero);
	    
	Atwist->val = v.twist;
	    pnl_fixact (Atwist);

	Agridc->val = v.colorgrid;
	    pnl_fixact (Agridc);

	Afringe->val = v.fringe;
	    pnl_fixact (Afringe);

	    if (Afringe->val){
		concave(1);
		gconfig();
	    }
	    else{
		concave(0);
		gconfig();
	    }
	    Fringe_on = Afringe->val;

	Ashading->val = v.shading; 
	    pnl_fixact (Ashading);
	    Shading = Ashading->val;
	

	/* light color: get max, use as brightness */
	tempf = 0.0;
	for (tempi=0 ; tempi < 3 ; ++tempi)
	    if(v.lightcol[tempi] > tempf) tempf = v.lightcol[tempi];
	Abright->val = tempf;
	pnl_fixact (Abright);

	Ared->val = v.lightcol[0] / tempf; 	
	pnl_fixact (Ared);
	Agrn->val = v.lightcol[1] / tempf; 	
	pnl_fixact (Agrn);
	Ablu->val = v.lightcol[2] / tempf; 	
	pnl_fixact (Ablu);

	Aambient->val = v.ambient;
	pnl_fixact (Aambient);

	Ashine->val = v.shine;
	pnl_fixact (Ashine);

	Asurface->val = v.surfonly;
	pnl_fixact (Asurface);

	Alight->val = v.lightson;
	pnl_fixact (Alight);

	Afocus->val = 0;
	pnl_fixact (Afocus);

	FROM_TO[TO][X] = (v.from_to[TO][X] - w.west) * XYscale;
	FROM_TO[TO][Y] = (v.from_to[TO][Y] - w.south) * XYscale;
	FROM_TO[TO][Z] = (v.from_to[TO][Z] - Z_Min_real) * Z_exag;
	FROM_TO[FROM][X] = (v.from_to[FROM][X] - w.west) * XYscale;
	FROM_TO[FROM][Y] = (v.from_to[FROM][Y] - w.south) * XYscale;
	FROM_TO[FROM][Z] = (v.from_to[FROM][Z] - Z_Min_real) * Z_exag;
	PNL_ACCESS (Point, Axy, x) = 
		(FROM_TO[FROM][X] - XBase)/XRange;
	PNL_ACCESS (Point, Axy, y) = 
		(FROM_TO[FROM][Y] - YBase)/YRange;
	pnl_fixact (Axy);

	Afollow->val = 0;
	pnl_fixact (Afollow);

	LightPos[X] = (v.lightpos[X] - w.west) * XYscale;
	LightPos[Y] = (v.lightpos[Y] - w.south) * XYscale;
	PNL_ACCESS(Point, Alightxy, x)=(LightPos[X]-XBase) / XRange;
	PNL_ACCESS(Point, Alightxy, y)=(LightPos[Y]-YBase) / YRange;
	pnl_fixact (Alightxy);
	LightPos[Z] = (v.lightpos[Z] - Z_Min_real) * Z_exag;
	LightPos[W] = v.lightpos[W];


	/* need to figure out inclination and view direction from FROM_TO 
	 * and set dials & vals for Aincl & Alook, respectively */
	
	get_direction (&view_dir, &incl_dir, FROM_TO);

	Alook->val = view_dir;
	pnl_fixact (Alook);
	Aincl->val = incl_dir;
	pnl_fixact (Aincl);
       
	_update_zbounds ();   /*  sets new ZRange  */

	Aheight->val = (FROM_TO[FROM][Z] - ZBase)/ ZRange;
	pnl_fixact (Aheight);

	Altheight->val = (LightPos[Z] - ZBase) / ZRange;
	pnl_fixact (Altheight);

	update_range ();
	  /* calls _update_zbounds, _update_height, update_exag, 
	   * _update_view_dir,
	   * transform_fromto, update_projection, and do_fast_display */


    }
    else
	fprintf(stderr,"...no settings loaded\n");

}



get_direction (view_dir, incl_dir, vect)
    float *view_dir, *incl_dir;
    float vect[2][4];
{
    float dx, dy, dz;
    float costheta, theta, adjacent;

    dx = vect[TO][X] - vect[FROM][X];
    dy = vect[TO][Y] - vect[FROM][Y];
    dz = vect[TO][Z] - vect[FROM][Z];

#ifdef PR_NEW
fprintf(stderr,"<dx,dy,dz> = %f,%f,%f\n",dx,dy,dz); 
#endif

/* project vector <dx,dy,dz> onto plane of constant z containing
 * the unit_from_to vector <1000,0,0>
 * final value should be 0.0 to 3600.0 */
    
    if (dx == 0 && dy == 0) *view_dir = 0.;
    else{ 
	if (dx == 0)
	    theta = 90.0;
	else{
	    costheta = dx / fsqrt(dx*dx + dy*dy);
	    theta = facos(costheta) * 180.0 / PI;
	}
	if (dy < 0)
	    theta = 360.0 - theta;
	*view_dir = theta * 10.;
    }

/* project vector <dx,dy,dz> onto plane of constant y containing
 * the unit_from_to vector <1000,0,0>
 * final value should be -900.0 (looking up) to 900.0 (looking down)*/
    
    if (dz == 0)
	theta = 0.0;
    else if ( dx == 0 && dy == 0 )
	theta = 90.0;
    else {
	adjacent = fsqrt(dx*dx + dy*dy);
	costheta = adjacent / fsqrt(adjacent*adjacent + dz*dz);
	theta =  facos(costheta) * 180.0 / PI;
    }
    if (dz > 0)
	theta =  -theta;
    
    *incl_dir = theta * 10.;
}



/* calculates normalized direction for a vector, given two pts */

get_norm_direction(dir, vect)
float dir[3], vect[2][4];
{
float normalizer, dx, dy, dz;

    dx = vect[TO][X] - vect[FROM][X];
    dy = vect[TO][Y] - vect[FROM][Y];
    dz = vect[TO][Z] - vect[FROM][Z];
    
    if(dx || dy || dz){
	normalizer = fsqrt(dx*dx + dy*dy + dz*dz);
	dir[X] = dx/normalizer;
	dir[Y] = dy/normalizer;
	dir[Z] = dz/normalizer;
    }
    else
	dir[X] = dir[Y] = dir[Z] = 0;

}

pr_fromto(msg)     /* for debugging  */
char msg[];
{
fprintf(stderr,"%s...\n",msg);
fprintf(stderr,"FROM: %f, %f, %f\n",FROM_TO[FROM][X],FROM_TO[FROM][Y],
		FROM_TO[FROM][Z]);
fprintf(stderr,"TO: %f, %f, %f\n",FROM_TO[TO][X],FROM_TO[TO][Y],
		FROM_TO[TO][Z]);
fprintf(stderr,"Aincl->val = %f\n",Aincl->val);
fprintf(stderr,"Alook->val = %f\n",Alook->val);
fprintf(stderr,"Atwist->val = %f\n",Atwist->val);

}


