/*
 *   d.window
 *
 *   Get window through graphics
 */

#include "gis.h"

main(argc, argv)
    int argc ;
    char **argv ;
{
    char window_name[64] ;
    struct Cell_head window ;
    int t, b, l, r ;

/* Initialize the GIS calls */
    G_gisinit(argv[0]) ;

    R_open_driver();

    if (D_get_cur_wind(window_name))
	G_fatal_error("No current graphics window") ;

    if (D_set_cur_wind(window_name))
	G_fatal_error("Current graphics window not available") ;

/* Read in the map window associated with window */
    G_get_window(&window) ;
    D_check_map_window(&window) ;

    if (D_check_map_window(&window))
	G_fatal_error("Setting map window") ;

    if (G_set_window(&window) == -1) 
	G_fatal_error("Current window not settable") ;

/* Determine conversion factors */
    if (D_get_screen_window(&t, &b, &l, &r))
	G_fatal_error("Getting graphics window") ;
    if (D_do_conversions(&window, t, b, l, r))
	G_fatal_error("Error in calculating conversions") ;

/* Do the digitizing */
    exit(window_work()) ;

    R_close_driver();
}
