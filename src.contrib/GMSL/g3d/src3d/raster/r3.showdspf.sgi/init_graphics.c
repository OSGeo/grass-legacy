#include "vizual.h"
#include <device.h>


Matrix idmat = {1.0, 0.0, 0.0, 0.0,
	        0.0, 1.0, 0.0, 0.0,
	        0.0, 0.0, 1.0, 0.0,
	        0.0, 0.0, 0.0, 1.0};

float	trd; 

init_graphics(wname, D_spec, Window)
char *wname;
struct dspec *D_spec;
long Window[3];
{
    float	xd,yd,zd; 
    float 	aspect;


    foreground ();
    {
	/*open the window that draws the isosurfaces */
	prefposition (10, 300, 700, 1000);
	aspect = 1.;
	Window[1] = winopen(wname);
	winconstraints ();
	/*  THIS WILL HAVE  TO BE DOUBLE BUFFERED  IF ROTATING */          
	doublebuffer();
	RGBmode();
	gconfig();
    }

    {
	/* open the window that draws the colortable */
	prefposition (1140,1270,10,1015);
	Window[2] = winopen("colortable");
	winconstraints ();
	RGBmode();
	singlebuffer();
	gconfig();  
	zbuffer (1);
	ortho2(0,100,0,1000);
	reshapeviewport ();
	sleep (1);
    }

/* TODO add a redraw handler for color window BB */
#ifdef SAMPLE
get_redraw(win)
short win;  /* use zero for any, a win number if you want others requeued */
{
Device dev;
short data;

    if (qtest()==REDRAW) {
	/* consuming a REDRAW */
        dev=qread(&data);
	if(win){
	    if(data == win) return (data);
	    else {
		/* requeueing a REDRAW */
		qenter(dev,data);
		return 0;
	    }
	}
	else
	    return(data);
    } else return 0;

}
#endif

     
    winset(Window[1]);
    zbuffer(TRUE);
    lsetdepth(0x0000,0x7fffff);

    mmode(MVIEWING);
    perspective(450,aspect,.10,1000.0);
    
    loadmatrix(idmat);

    /* want to pick a logical distance from origin so base it on dimensions
    ** of the data
    */
    xd = Headfax.xdim * D_spec->xscale;
    yd = Headfax.ydim * D_spec->yscale;
    zd = Headfax.zdim * D_spec->zscale;

    /* pick greatest dimension to use for translation of viewer from origin*/
    if(xd  < yd)
	trd  = yd;
    else
	trd = xd;
    if(trd < zd)
	trd = zd;
    
    translate (0.0,0.0,-trd*1.6);	/* move it away from eye */

    do_lights();
    clear_screen();
    winset(Window[2]);
    clear_screen();
    winset(Window[1]);

}
