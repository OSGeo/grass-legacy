/*
 * Start up graphics processing.  Anything that needs to be assigned, set up,
 * started-up, or otherwise initialized happens here.  This is called only at
 * the startup of the graphics driver.
 *
 * The external variables define the pixle limits of the graphics surface.  The
 * coordinate system used by the applications programs has the (0,0) origin
 * in the upper left-hand corner.  Hence,
 *    SCREEN_LEFT < SCREEN_RIGHT
 *    SCREEN_TOP  < SCREEN_BOTTOM 
 *
 * NCOLORS is set to the total number of colors available on the device.  This
 * most certainly needs to be more than 100 (or so).  If you are writing a
 * driver with fewer colors you probably need to provide your own Color(),
 * Color_table_float(), Color_table_fixed(), and
 * Reset_color() routines (see ../lib/{Color.c,Reset_clr.c,Clr_table.c}).
 */

#include <gl.h>
#include <device.h>
#include <stdio.h> /* DEBUG*/

int SCREEN_LEFT	  	= 0;
int SCREEN_RIGHT  	= 0;
int SCREEN_BOTTOM 	= 0;
int SCREEN_TOP    	= 0;
int NCOLORS   		= 3840;

char *malloc ();

static int size_changed = -1;
static long *Parray = NULL;
static long x_size, y_size;

short *Raster_Buffer = NULL;

Graph_Set() 
{
    setpgrp ();	/* avoid signal problems */

    if (0 > winopen ("GRASS Graphics Window"))
	fprintf (stderr, "Cannot open graphics window\n"), exit (-1);
    concave (1);
    gconfig ();

    /*
    NCOLORS = pow (2., (double) getplanes ()) - COLOR_OFFSET;
    */
    _setsize ();
    color (CYAN);
    clear();

    overlay (2);
    gconfig ();
    drawmode (OVERDRAW);
    mapcolor (2, 255, 255, 255);
    drawmode (NORMALDRAW);

    qdevice (REDRAW);
    /*
    qdevice (REDRAWICONIC);
    */
}

_setsize ()
{
    long tmp_x, tmp_y;

    if (Parray == NULL)	/* first time */
    {
	reshapeviewport ();
	getsize (&x_size, &y_size);
	ortho2 (-.5, x_size-.5, -.5, y_size-.5);
	size_changed = 1;
    }
    else 
    {
	reshapeviewport ();
	getsize (&tmp_x, &tmp_y);
	if (tmp_x != x_size || tmp_y != y_size)
	{
/*DEBUG*/  fprintf (stderr, "SIZE CHANGE  %d,%d    %d,%d\n", x_size, y_size, tmp_x, tmp_y);
	    size_changed = 1;
	    x_size = tmp_x;
	    y_size = tmp_y;
	}
	ortho2 (-.5, tmp_x-.5, -.5, tmp_y-.5);
    }

    if (size_changed)
    {
	if (Raster_Buffer != NULL)
	    free (Raster_Buffer);
	Raster_Buffer = (short *) malloc (x_size * sizeof (short));
	if (Raster_Buffer == NULL)
	    fprintf (stderr, "Driver Malloc out of memory\n"), exit (0);

	if (Parray != NULL)
	    free (Parray);
	Parray = (long *) malloc (x_size * y_size * sizeof (long));

	if (Parray == NULL)
	    fprintf (stderr, "Driver Malloc out of memory\n"), exit (0);

	clear_all_pads ();
	SCREEN_RIGHT = x_size - 1;
	SCREEN_BOTTOM = y_size - 1;
	SCREEN_LEFT	  	= 0;
	SCREEN_TOP    	= 0;

	color (CYAN);
	clear ();
	size_changed = 0;
	
    }
    else
    {
	long x, y;
	getorigin (&x, &y);

	/* rect stuff MUST be inside screen */
	if (x < 0 || y < 0 || 
	    (x+x_size-1) > XMAXSCREEN || (y+y_size-1) > YMAXSCREEN)
	{
	    color (MAGENTA);
	    clear ();
	}
	else
	    lrectwrite (0, 0, x_size-1, y_size-1, Parray);
    }


#ifdef FOO
/*DEBUG*/      fprintf (stderr, "WINDOW (%d %d)  (%d %d)\n", SCREEN_LEFT, 
		SCREEN_BOTTOM, SCREEN_RIGHT, SCREEN_TOP);
#endif
}

_save_screen ()
{
    long x, y;
    getorigin (&x, &y);

    if (!(x < 0 || y < 0 || 
	(x+x_size-1) > XMAXSCREEN || (y+y_size-1) > YMAXSCREEN))
	    lrectread (0, 0, x_size-1, y_size-1, Parray);
}
