/*  %W%  %G%  */

/*
 *   Dgraph
 *
 *   Usage:  Dgraph [input] [color] [hsize] [vsize]
 *           Dgraph [input=name] [color=name] [hsize=num] [vsize=num]
 *
 *   Draw graphics in a graphics window.   Graph lines come from stdin,
 *      unless input specified.
 *
 */

#define MAIN
#include "options.h"
#include <stdio.h>
#define USAGE	"[input=name] [color=name] [hsize=num] [vsize=num]"

main(argc, argv)
	int argc ;
	char **argv ;
{
	char buff[128] ;
	char window_name[64] ;
	extern int stash_away() ;

/* Initialize the GIS calls */
	G_gisinit("Dgraph") ;

/* Check command line */
	set_default_options() ;

	if (D_parse_command(argc, argv, variables, n_variables, stash_away))
	{
		fprintf(stderr,"Usage: %s %s\n", argv[0], USAGE) ;
		exit(-1) ;
	}

	R_open_driver();

	if (D_get_cur_wind(window_name))
		G_fatal_error("No current window") ;

	if (D_set_cur_wind(window_name))
		G_fatal_error("Current window not available") ;

	if (D_get_screen_window(&t, &b, &l, &r))
		G_fatal_error("Getting screen window") ;

/* Finish graphics setup */
	R_set_window(t, b, l, r) ;
	R_standard_color(color) ;
	set_text_size() ;

/* Do the graphics */
	set_graph_stuff() ;
	graphics (stdin) ;

	R_close_driver();
}
