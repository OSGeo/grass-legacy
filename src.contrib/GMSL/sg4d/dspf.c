/*
Written by Bill Brown, Summer 1993
*/
/*
** Copyright USA CERL 1992. All rights reserved.
*/
/*
#include "gis.h"
*/
#include "externs.h"
#include "math.h"

void do_dspf_redraw();
void do_dspf_rescale();
void do_setdspf();
extern void do_clear();

static int Dspf_enabled=0;
/*
#define SCALE_NORMS
*/

int
show_dspf()
{
    return(Dspf_enabled? Ashowdspf->val: 0);
}

void
do_newdspf()
{
int argc = 1;
static char grdname[100], dspname[100], clrname[100];
static char *argv[4];
static int first = 1;
int ret;
    
    if(first){
	dspf_set_redraw(do_dspf_redraw);
	dspf_set_scalefunc(do_dspf_rescale);
	dspf_set_clear(do_clear);
	/*
	dspf_set_clear(do_dspf_redraw);
	*/
	do_lights(0);
	first = 0;
    }

    do{
	fprintf(stderr,"\nEnter name of grid3 file: ");
    } while (!G_gets(grdname));
    fprintf(stderr,"\n");
    if(grdname[0]) argc ++;
    argv[1] = grdname;

    do{
	fprintf(stderr,"\nEnter name of display file: ");
    } while (!G_gets(dspname));
    fprintf(stderr,"\n");
    if(dspname[0]) argc ++;
    argv[2] = dspname;

    do{
	fprintf(stderr,"\nEnter name of color file: ");
    } while (!G_gets(clrname));
    fprintf(stderr,"\n");
    if(clrname[0]) argc ++;
    argv[3] = clrname;

    ret = dspf_load(argc, argv);
    Dspf_enabled = (ret < 0? 0: 1);
    
    if(Dspf_enabled){
/*  complicates scripting
	Ashowdspf->val = 1;
	pnl_fixact (Ashowdspf);
*/
	do_setdspf();
    }
    else
	fprintf(stderr,"ERROR loading files.\n");

}

static int saveshade;

push_dspf_lighting()
{
	
#ifdef XS24
    saveshade = getsm();
    shademodel(FLAT);	
#endif
    do_dspf_lights(1);
    set_transparency();
}

pop_dspf_lighting()
{
	
#ifdef XS24
    shademodel(saveshade);
#endif
    unset_transparency();
    do_dspf_lights(0);
}

void
do_setdspf()
{
int ret;

    if(Dspf_enabled){
	push_dspf_transforms();
	push_dspf_lighting();

	ret = dspf_set();
	Dspf_enabled = (ret < 0? 0: 1);

	pop_dspf_lighting();
	pop_dspf_transforms();
    }
    else{
	fprintf(stderr,"Need to load files first.\n");
    }
}

void
do_drawdspf()
{
    push_dspf_transforms();
    push_dspf_lighting();
    dspf_do_last_draw();
    pop_dspf_lighting();
    pop_dspf_transforms();
}

void
do_dspf_redraw()
{
Matrix m;

    getmatrix(m);
    popmatrix();
    do_fast_display();
    pushmatrix();
    loadmatrix(m);

}

void
do_dspf_rescale()
{
    pop_dspf_transforms();
    push_dspf_transforms();
}

pop_dspf_transforms()
{
    popmatrix();
}

push_dspf_transforms()
{
float o_west, o_south, o_bot, dspf_zscale, tw, ts, tup, xres, yres, zres;

    pushmatrix();
    dspf_get_res(&xres, &yres, &zres);
    if(0 <= dspf_getorigin(&o_west, &o_south, &o_bot)){
	o_south += yres;
	tw = (o_west-X_Min_real)*XYscale;
	ts = (o_south-Y_Min_real)*XYscale;
	dspf_get_zscale(&dspf_zscale);
	/*
	o_bot *= dspf_zscale;
	*/
	tup = (o_bot-Z_Min_real)*Z_exag;
	translate(tw, ts, tup);
    }
    scale(xres*XYscale, yres*XYscale, zres*dspf_zscale*Z_exag);

#ifdef SCALE_NORMS
    /* exaggerate normals in Z direction */
    dspf_set_ZNexag((float)(Z_exag/XYscale));
fprintf(stderr,"normal exag factor: %f\n", (float)(Z_exag/XYscale));
#endif
}

draw_dspf_bbox()
{
    if(Dspf_enabled){
	push_dspf_transforms();
/* 
do_dspf_lights(1);
DEBUG */
	dspy_bbox();
/* 
do_dspf_lights(0);
DEBUG */
	pop_dspf_transforms();
    }
}

