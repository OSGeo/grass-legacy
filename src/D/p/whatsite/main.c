/* %W%  %G%  */

/*
 *   Dwhatsite
 *
 *   Usage:  Dwhatsite sitename [color1] [color2]
 *           Dwhatsite name=sitename color1=name color2=name
 *
 *   Interactive query of map for site information.
 */

#define USAGE	"name=sitename [color1=name] [color2=name]"
#include "gis.h"
#define MAIN
#include "sites.h"
#include "options.h"

main(argc, argv)
	int argc ;
	char **argv ;
{
	extern int stash_away() ;
	int t, b, l, r ;
	char *mapset ;
	char buff[128] ;
	char window_name[64] ;
	struct Cell_head window ;

/* Initialize the GIS calls */
	G_gisinit("site information") ;

/* Check command line */
	set_default_options() ;

	if (D_parse_command(argc, argv, variables, n_variables, stash_away))
	{
		fprintf(stderr,"Usage: %s %s\n", argv[0], USAGE) ;
		exit(-1) ;
	}

/* Make sure map is available */
	mapset = G_find_file ("site", name, "") ;
	if (mapset == NULL)
	{
		sprintf(buff,"Site file [%s] not available", name);
		G_fatal_error(buff) ;
	}

    R_open_driver();

	if (D_get_cur_wind(window_name))
		G_fatal_error("No current window") ;

	if (D_set_cur_wind(window_name))
		G_fatal_error("Current window not available") ;

	if (D_claim_type_is("map"))
		G_fatal_error("Current window not for maps") ;

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

/* Do initial display of sites */
	R_color(color1) ;
	draw_all_sites(mapset, name) ;

/* Interact */
	interact(mapset, name) ;

/* Quit */
    R_close_driver();
}
