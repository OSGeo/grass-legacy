/*
 *   Raghavan Srinivasan, Agricultural Engineering, Purdue University
 *   srin@ecn.purdue.edu  April 1991
 *
 *   zoom
 *
 *   Usage:  zoom
 * 
 *   Purpose of zoom is to zoom in or out an area of interest from the
 *   displayed cell map on the graphics monitor. It is highly interactive
 *   mouse driven program. The user can zoom or unzoom for any number of times
 *   until he/she satisfies. The unzoom will change the window to the
 *   last modified window and if one tries to unzoom once again it will
 *   change to default window.
 */

#define GLOBAL
#include "what.h"
#include "raster.h"
#include "display.h"
#include "local_proto.h"

int 
main (int argc, char **argv)
{
    char temp[128] ;
    int t, b, l, r ;
    struct Cell_head window;

/* Initialize the GIS calls */
    G_gisinit (argv[0]) ;

    R_open_driver();

    if (D_get_cur_wind(temp))
	G_fatal_error("No current graphics window") ;

    if (D_set_cur_wind(temp))
	G_fatal_error("Current graphics window not available") ;

/* Read in the map window associated with window */
    G_get_window(&window) ;

    if (D_check_map_window(&window))
	G_fatal_error("Setting graphics window") ;

    if (G_set_window(&window) == -1) 
	G_fatal_error("Can't set current graphics window") ;

/* Determine conversion factors */
    if (D_get_screen_window(&t, &b, &l, &r))
	G_fatal_error("Getting graphics window coordinates") ;
    if (D_do_conversions(&window, t, b, l, r))
	G_fatal_error("Error in calculating conversions") ;

    nlayers = 0;
    if (argc == 1)
    {
	if(D_get_cell_name (temp))
	    fprintf (stderr, "warning: no data layer drawn in current window\n");
	    nlayers++;
    }
    if (nlayers == 0)
	exit(0);

    R_close_driver();
    what (temp,&window);

    return 0;
}
