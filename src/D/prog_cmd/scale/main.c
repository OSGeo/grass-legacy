/*  %W%  %G%  */

/*
 *   Dscale
 *
 *   Usage:  Dscale color1 color2
 *           Dscale color1=num color2=num
 *
 *   Draw an "appropriate" scale on the map
 */

#include "gis.h"
#define MAIN
#include "options.h"

#define USAGE	"[color1=num] [color2=num]"

main(argc, argv)
	int argc ;
	char **argv ;
{
	char buff[128] ;
	char window_name[64] ;
	struct Cell_head window ;
	int i ;
	int t, b, l, r ;
	extern int stash_away() ;

/* Initialize the GIS calls */
	G_gisinit("Dscale") ;

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

/* Read in the map window associated with window */
	G_get_window(&window) ;

	if (D_check_map_window(&window))
		G_fatal_error("Setting map window") ;

	if (G_set_window(&window) == -1) 
		G_fatal_error("Current window not settable") ;

/* Determine conversion factors */
	if (D_get_screen_window(&t, &b, &l, &r))
		G_fatal_error("Getting screen window") ;
	if (D_do_conversions(&window, t, b, l, r))
		G_fatal_error("Error in calculating conversions") ;

/* Draw the scale */
	draw_scale(color1, color2) ;

/* Add this command to list */
	strcpy(buff, argv[0]) ;
	for(i=1; i<argc; i++)
	{
		strcat(buff, " ") ;
		strcat(buff, argv[i]) ;
	}
	D_add_to_list(buff) ;

    R_close_driver();
}
