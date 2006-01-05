/*
 * This program get's the current Xserver resolution and calculates the map
 * scale of map displayed in GRASS Monitor from monitor settings, zoom level
 * etc.
 *
 * Ideas taken from d.barscale and xdpyinfo.c
 *
 * Markus Neteler
 * neteler geog.uni-hannover.de
 *
 * This software is released under GNU GPL >= 2
 ***************************************************************
 * TODO: make LatLong working
 *
 */

/*uncomment to get much more output */
/*
 * #define DEBUG
 */

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <X11/Xlib.h>
#include "gis.h"
#include "display.h"
#include "D.h"
#include "raster.h"
#include "glocale.h"

#ifdef X11
int screeninfo(void)
{
    char window_name[255];
    struct Cell_head window;
    int t, b, l, r;		/* rows/cols settings of monitor */
    double s_rows, s_cols;	 /* rows/cols settings of displayed area */
    double ns_meters, we_meters;
    double Xres, Yres, ns_Xres, we_Xres, av_scale;
    Display *dpy;		/* X connection */
    char *displayname = NULL;	 /* server to contact */
    int scr;
    int dlevel=3; /* debug level */
    char *name;

    if ((name = G__getenv("MONITOR")) == NULL)
        G_fatal_error (_("No monitor currently selected for output"));

    G_get_window(&window);

    /* TODO: get over code from g.region -m */
    if ((G_projection() == PROJECTION_LL))
	   G_fatal_error( "No support for latitude-longitude data base yet");

    if (D_get_cur_wind(window_name))
	G_fatal_error("No current window");

    if (D_set_cur_wind(window_name))
	G_fatal_error("Current window not available");

    /* Read in the map window associated with window */
    we_meters = window.east - window.west;
    ns_meters = window.north - window.south;

    if (D_check_map_window(&window))
	G_fatal_error("Setting map window");

    if (G_set_window(&window) == -1)
	G_fatal_error("Current window not settable");

    /* Determine conversion factors */
    if (D_get_screen_window(&t, &b, &l, &r))
	G_fatal_error("Getting screen window");

    if (D_do_conversions(&window, t, b, l, r))
	G_fatal_error("Error in calculating conversions");

    s_cols = D_get_d_east() - D_get_d_west();
    s_rows = D_get_d_south() - D_get_d_north();

    /* get from Xserver, x and y differ in Xserver!! */
    dpy = XOpenDisplay(displayname);
    if (!dpy)
	G_fatal_error ( "Unable to open display %s", XDisplayName(displayname));
	/* server name */
    scr = 0;			/* check first screen */
    Xres = ((((double)DisplayWidth(dpy, scr)) * 25.4) /
	    ((double)DisplayWidthMM(dpy, scr)));
    Yres = ((((double)DisplayHeight(dpy, scr)) * 25.4) /
	    ((double)DisplayHeightMM(dpy, scr)));


    XCloseDisplay(dpy);

    /* recalc screen resolution in dot/meter */
    ns_Xres = (Xres / 2.54) * 100.0;
    we_Xres = (Yres / 2.54) * 100.0;

    G_debug(dlevel, "True map size:  we_meters: %f ns_meters: %f",
	    we_meters, ns_meters);
    G_debug(dlevel, "monitor size: screen_we_pix: %i screen_ns_pix: %i",
	    r - l, b - t);

    G_debug(dlevel, "Pixel numbers used in screen: ns: %f  we:%f", s_rows,
	    s_cols);
    G_debug(dlevel, "Map rows: %i  map cols: %i\n", window.rows, window.cols);
    G_debug(dlevel, "Xserver screen resolution: x:%f dpi y:%f dpi\n", Xres, Yres);

    G_debug(dlevel, " Map height in true centimeter on screen: %f",
	    s_rows / ns_Xres * 100);
    G_debug(dlevel, " Map width in true centimeter on screen:  %f",
	    s_cols / we_Xres * 100);
    G_debug(dlevel, " (note: deviations may arise from your screen adjustments)");

    G_debug(dlevel, " Current map scale (note: Xserver x_res != y_res):");
    G_debug(dlevel, "   y-scale: 1:%f", ns_meters / (s_rows / ns_Xres));
    G_debug(dlevel, "   x-scale: 1:%f", we_meters / (s_cols / we_Xres));

    /* calculate average scale as Xdpi might differ in x and y direction
     * as depending on video card: */
    av_scale =
	(ns_meters / (s_rows / ns_Xres) + we_meters / (s_cols / we_Xres)) / 2.;
    /* put it out rounded: */
    G_message( _("Approximate map scale in GRASS monitor <%s> 1:%d"), name, (int)floor(av_scale/1000. + 0.5) * 1000);

    return (0);
}

#endif

