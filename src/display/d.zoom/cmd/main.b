/*
 *   d.zoom
 *
 *   Get region through graphics
 */

#include "gis.h"

main(argc, argv)
    int argc ;
    char **argv ;
{
    char window_name[64] ;
    struct Cell_head window ;
    int t, b, l, r ;
    int stat;
    struct Flag *quiet, *rotate;

/* Initialize the GIS calls */
    G_gisinit(argv[0]) ;

    quiet = G_define_flag();
    quiet->key = 'q';
    quiet->description = "Quiet";

    rotate = G_define_flag();
    rotate->key = 'r';
    rotate->description = "Rotate instead of zoom (lat-lon only)";

    if (argc > 1 && G_parser(argc,argv))
	exit(1);

    R_open_driver();

    if (D_get_cur_wind(window_name))
	G_fatal_error("No current graphics frame") ;

    if (D_set_cur_wind(window_name))
	G_fatal_error("Current graphics frame not available") ;

/* Read in the map window associated with window */
    G_get_window(&window) ;
    D_check_map_window(&window) ;

    if (D_check_map_window(&window))
	G_fatal_error("Setting map region") ;

    if (G_set_window(&window) == -1) 
	G_fatal_error("Current region not settable") ;

/* Determine conversion factors */
    if (D_get_screen_window(&t, &b, &l, &r))
	G_fatal_error("Getting graphics frame") ;
    if (D_do_conversions(&window, t, b, l, r))
	G_fatal_error("Error in calculating conversions") ;

/* Do the zoom */
    stat = zoom(quiet->answer, rotate->answer) ;

    R_close_driver();

    exit(stat);
}
