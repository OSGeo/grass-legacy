/*
 * $Id*
 *
 * This program get's the current Xserver resolution and calculates the map
 * scale of map displayed in GRASS Monitor from monitor settings, zoom level
 * etc.
 *
 * Ideas taken from d.barscale and xdpyinfo.c
 *
 * Markus Neteler
 * neteler@geog.uni-hannover.de
 *
 * This software is released under GNU GPL >= 2
 ***************************************************************
 *
 * TODO: Print current monitor name (x0, x1, ...)
 *
 */

/*uncomment to get much more output */
/*
#define DEBUG
*/

#include "gis.h"
#include "display.h"
#include "D.h"
#include "raster.h"
#include <math.h>
#include <X11/Xlib.h>
#include <stdio.h>

int screeninfo (void)
{
	char window_name[255] ;
	struct Cell_head window ;
	int t, b, l, r ;         /* rows/cols settings of monitor */
	double s_rows, s_cols ; /* rows/cols settings of displayed area */
	double ns_meters, we_meters;
	double Xres, Yres, ns_Xres, we_Xres, av_scale;
	Display *dpy; /* X connection */
	char *displayname = NULL;           /* server to contact */
	int scr;

	{
		struct Cell_head W ;
		G_get_window(&W) ;
		if (W.proj == PROJECTION_LL)
		{
		  fprintf(stderr,"\nSorry, this module does now work with a latitude-longitude data base.\n");
		  exit(-1);
		}
	}

	if (D_get_cur_wind(window_name))
		G_fatal_error("No current window") ;

	if (D_set_cur_wind(window_name))
		G_fatal_error("Current window not available");

	/* Read in the map window associated with window */
	G_get_window(&window);
        we_meters  = window.east - window.west;
        ns_meters  = window.north - window.south;

	if (D_check_map_window(&window))
		G_fatal_error("Setting map window");

	if (G_set_window(&window) == -1)
		G_fatal_error("Current window not settable");

	/* Determine conversion factors */
	if (D_get_screen_window(&t, &b, &l, &r))
		G_fatal_error("Getting screen window") ;

	if (D_do_conversions(&window, t, b, l, r))
		G_fatal_error("Error in calculating conversions") ;

        s_cols = D_get_d_east() - D_get_d_west();
        s_rows = D_get_d_south() - D_get_d_north();
        
        /* get from Xserver, x and y differ in Xserver!! */
        dpy = XOpenDisplay (displayname);
        if (!dpy) {
                   fprintf (stderr, "Unable to open display \"%s\".\n",
                            XDisplayName (displayname)); /* server name */
                   exit (1);
                  }
        scr=0; /* check first screen */
        Xres = ((((double) DisplayWidth(dpy,scr)) * 25.4) /
               ((double) DisplayWidthMM(dpy,scr)));
        Yres = ((((double) DisplayHeight(dpy,scr)) * 25.4) /
               ((double) DisplayHeightMM(dpy,scr)));

#ifdef DEBUG
	fprintf(stderr,"Xserver screen resolution: x:%f dpi y:%f dpi\n", Xres, Yres);
#endif

        XCloseDisplay (dpy);

        /* recalc screen resolution in dot/meter */
        ns_Xres=(Xres / 2.54) * 100.0;
        we_Xres=(Yres / 2.54) * 100.0;

        fprintf(stderr,"Scale of map displayed in GRASS monitor: \n"); /* add monitor name here */

#ifdef DEBUG
	fprintf(stderr,"\n True map size:  we_meters: %f ns_meters: %f \n", we_meters, ns_meters);
	fprintf(stderr," monitor size: screen_we_pix: %i screen_ns_pix: %i\n", r-l, b-t);

	fprintf(stderr," Pixel numbers used in screen: ns: %f  we:%f \n", s_rows, s_cols);
	fprintf(stderr," Map rows: %i  map cols: %i\n", window.rows, window.cols );

	fprintf(stderr,"\n Map height in true centimeter: %f\n", s_rows/ns_Xres*100);
	fprintf(stderr," Map width in true centimeter:  %f\n", s_cols/we_Xres*100);
	fprintf(stderr," (note: deviations may arise here from your screen adjustments)\n");
	
	fprintf(stderr,"\n Current map scale (note: Xserver x_res != y_res):\n");
	fprintf(stderr,"   y-scale: 1:%f\n", ns_meters/(s_rows/ns_Xres));
	fprintf(stderr,"   x-scale: 1:%f\n", we_meters/(s_cols/we_Xres));
#endif

 	/* calculate average scale as Xdpi might differ in x and y direction
 	 * as depending on video card: */
 	av_scale=(ns_meters/(s_rows/ns_Xres) + we_meters/(s_cols/we_Xres))/2.;
 	/* put it out rounded: */
 	fprintf(stderr,"   Map scale: 1:%i\n", (int)floor (av_scale + 0.5));

 	return(0);
}
