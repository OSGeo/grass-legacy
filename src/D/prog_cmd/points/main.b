/*
 *   Dpoints
 *
 *   Usage:  Dpoints color=name size=num type=x/diamond/box/+
 *
 *   Draws small marks at points listed in stdin.   
 */

#define USAGE1	"color=name size=num type=x/diamond/box/+"
#define USAGE2	"points, X Y, provided in stdin"
#include "gis.h"

#define MAIN
#include "options.h"

main(argc, argv)
	int argc ;
	char **argv ;
{
	char buff[128] ;
	char window_name[64] ;
	struct Cell_head window ;
	extern int stash_away() ;
	int i ;
	int t, b, l, r ;

/* Initialize the GIS calls */
	G_gisinit(argv[0]) ;

/* Check command line */
	set_default_options() ;

	if (D_parse_command(argc, argv, variables, n_variables, stash_away))
	{
		fprintf(stderr,"Usage: %s %s\n", argv[0], USAGE1) ;
		fprintf(stderr,"       %s\n", USAGE2) ;
		exit(-1) ;
	}

/* Setup driver and check important information */
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

/* Do the plotting */
	R_standard_color (color) ;
	switch(type)
	{
	case TYPE_X:
		draw_points_x(size,&window) ;
		break ;
	case TYPE_PLUS:
		draw_points_plus(size,&window) ;
		break ;
	case TYPE_BOX:
		draw_points_box(size,&window) ;
		break ;
	case TYPE_DIAMOND:
		draw_points_diamond(size,&window) ;
		break ;
	}

	if (infile != stdin)
	{
	/* Add this command to list */
		strcpy(buff, argv[0]) ;
		for(i=1; i<argc; i++)
		{
			strcat(buff, " ") ;
			strcat(buff, argv[i]) ;
		}
		D_add_to_list(buff) ;
	}

	R_close_driver();
}
