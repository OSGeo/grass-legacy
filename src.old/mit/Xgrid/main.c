/*  %W%  %G%  */

/*
 *   Xgrid
 *
 *   Usage:  Xgrid size=num color=name
 *
 *   Draw a grid on top of the displayed image.
 */

#define USAGE	"size=num color=name"
#include "gis.h"
#define MAIN
#include "options.h"

struct Cell_head window;

main(argc, argv)
	int argc ;
	char **argv ;
{
	char buff[128] ;
	char window_name[64] ;
	int i ;
	int t, b, l, r ;
	extern int stash_away(), set_default_options() ;


/* Initialize the GIS calls */
	G_gisinit("Xgrid") ;


/* Check command line */
	set_default_options() ;


	if (D_parse_command(argc, argv, variables, n_variables, stash_away))
	{
		fprintf(stderr,"Usage: %s %s\n", argv[0], USAGE) ;
		exit(-1) ;
	}

/*
	if (D_get_cur_wind(window_name))
		G_fatal_error("No current window") ;

	if (D_set_cur_wind(window_name))
		G_fatal_error("Current window not available") ;
*/

/* Read in the map window associated with window */
	G_get_window(&window) ;
/*
	if (D_check_map_window(&window))
		G_fatal_error("Setting map window") ;

	if (G_set_window(&window) == -1) 
		G_fatal_error("Current window not settable") ;
*/
/* Determine conversion factors 
	if (D_get_screen_window(&t, &b, &l, &r))
		G_fatal_error("Getting screen window") ;
	if (D_do_conversions(&window, t, b, l, r))
		G_fatal_error("Error in calculating conversions") ;
*/


/* Do the plotting */
	draw_grid(size, color) ;

/* Add this command to list 
	strcpy(buff, argv[0]) ;
	for(i=1; i<argc; i++)
	{
		strcat(buff, " ") ;
		strcat(buff, argv[i]) ;
	}
	D_add_to_list(buff) ;
*/

}
