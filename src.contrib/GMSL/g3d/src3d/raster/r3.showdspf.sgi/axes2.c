/* Written by Mike Krogh, NCSA, June 20, 1989                      */
/* Modified by Dave Gerdes  Feb 1990 */
/*
** Program was modified to avoid the undesirable effects
** that occur if a mouseclick event is lost.
*/

#include <stdio.h>
#include <GL/gl.h>
#include <device.h>
#include "vizual.h"

static int Gxrot = 0;
static int Gyrot = 0;
static int Gzrot = 0;
static float Gztrans = 0;


rotate_model (D_spec)
    struct dspec *D_spec;
{
    short value;
    int dev;

    init_rotate ();  /* initialize the graphics system */

    /* if specs have changed externally, then need to make them jive */
    Gxrot = D_spec->xrot * 10;
    Gyrot = D_spec->yrot * 10;
    Gzrot = D_spec->zrot * 10;
    Gztrans = D_spec->ztrans;
    D_spec->zrot = 0;	/* Z is not used, so zero it so they jive */

    for (;;)
    {
	while (qtest ())     /* check for an event */
	{
	    switch (dev = qread (&value)) {    /* get the event */
		case REDRAW: 		/* the screen needs refreshing */
		    reshapeviewport ();
		    break;
		case ESCKEY: 		/* DONE: Escape key was pressed */
		case RIGHTMOUSE: 	/* or right mouse button */
		    if (value)		/* only on down stroke */
		    {
			reset_rotate();
			return (0);
		    }
		    break;
		case LEFTMOUSE:	 	
		    if (value)		/* only on down stroke */
			rotate_draw (D_spec);
		    break;
		case MIDDLEMOUSE: 
		    if (value)		/* only on down stroke */
			translate_draw (D_spec);
		    break;
	    }
	}   /*  end while qtest or not attached  */

	drawscene (D_spec, 0,0,0);   /* update the screen */
    }

    /*NOTREACHED*/
}



init_rotate ()
{
    int gid;
    float aspect;

    doublebuffer ();
    /* reconfigure the hardware for double buffering */
    gconfig ();

    /* interested in the following events */
    qdevice (ESCKEY);
    qdevice (RIGHTMOUSE);
    qdevice (REDRAW);
    qdevice (MIDDLEMOUSE);
    qdevice (LEFTMOUSE);
    /* start off with a redraw event for our window */
    qenter  (REDRAW, gid);
}

reset_rotate ()
{
    unqdevice (ESCKEY);
    unqdevice (MIDDLEMOUSE);
    unqdevice (LEFTMOUSE);
    unqdevice (RIGHTMOUSE);
}



rotate_draw (D_spec)
    struct dspec *D_spec;
/* get the mouse location for rotation control */
{

    int xmouse,ymouse;
    static int oxmouse,oymouse;
    static int first;
    int dxmouse,dymouse;
    short value;
    int dev;

    int xorg,yorg;
    int xsize,ysize;

    first = 1;
    while (getbutton (LEFTMOUSE)) 
    {
	/* get x & y rotation only */
	xmouse = getvaluator (MOUSEX);
	ymouse = getvaluator (MOUSEY);
	if (first) 
	{
	    first = 0;
	    oxmouse = xmouse;
	    oymouse = ymouse;
	}
	/* figure out how much the mouse moved from last time */
	dxmouse= 5 * (xmouse-oxmouse);
	dymouse= -5 * (ymouse-oymouse);
	oxmouse = xmouse;
	oymouse = ymouse;

	/* draw the scene */
	drawscene (D_spec, dymouse,dxmouse,0);

    }
}


/* translate in z direction */
translate_draw (D_spec)
    struct dspec *D_spec;
{

    int xmouse;
    static int oxmouse;
    static int first;
    int dxmouse;
    short value;
    int dev;

    int xorg,yorg;
    int xsize,ysize;

    first = 1;
    while (getbutton (MIDDLEMOUSE))
    {
	xmouse = getvaluator (MOUSEX);
	if (first) 
	{
	    first = 0;
	    oxmouse = xmouse;
	}
	dxmouse = xmouse-oxmouse;
	oxmouse = xmouse;

	drawscene (D_spec, 0,0,dxmouse);
    }
}



/* this will update the screen and update rotation and translation */
/* if necessary */
drawscene (D_spec, xdelta,ydelta,zdelta)
    struct dspec *D_spec;
    int xdelta,ydelta,zdelta;
{
    static int first = 1;
    static double x, y, z;

    if (first)
    {
	x = Headfax.xdim*D_spec->xscale/2;
	y = Headfax.ydim*D_spec->yscale/2;
	z = Headfax.zdim*D_spec->zscale/2;
    }

    pushmatrix ();
	/* clear the window to black and reset the z buffer */

	clear_screen();


	/* update the total rotational and translational amounts */
	Gxrot += xdelta;
	Gyrot += ydelta;
	Gztrans += (float)zdelta;


	D_spec->xrot = Gxrot/10.;
	D_spec->yrot = Gyrot/10.;
	D_spec->zrot = Gzrot/10.;
	D_spec->ztrans = Gztrans;

	translate (0.0,0.0,Gztrans); 
	rotate (Gyrot,'y');
	rotate (Gzrot,'z');
	rotate (Gxrot,'x');


	translate (-x,-y,-z);
	
	do__bbox (D_spec);

	/* restore the saved viewing matrix */
    popmatrix ();
    /* swap the front and back buffers to show new scene */
    swapbuffers ();
}


/* Draw axes */
drawobject ()
{
    float v[3];

    cpack (0xff0000);
    bgnline ();
    v[0] = 0.0; v[1] = 0.0; v[2] = 0.0;
    v3f (v);
    v[0] = 5.0; v[1] = 0.0; v[2] = 0.0;
    v3f (v);
    endline ();

    cpack (0x0000ff);
    bgnline ();
    v[0] = 0.0; v[1] = 0.0; v[2] = 0.0;
    v3f (v);
    v[0] = 0.0; v[1] = 5.0; v[2] = 0.0;
    v3f (v);
    endline ();

    cpack (0xffffff);
    bgnline ();
    v[0] = 0.0; v[1] = 0.0; v[2] = 0.0;
    v3f (v);
    v[0] = 0.0; v[1] = 0.0; v[2] = 5.0;
    v3f (v);
    endline ();
}
