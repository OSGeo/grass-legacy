/*  %W%  %G%  */

#define MAIN
#include "options.h"
#include "gis.h"
/*
 *   d.colors
 *
 *   Usage:  d.colors
 *
 */

#define USAGE	"[name=mapname]"

main(argc, argv)
	char **argv ;
{
	struct Cell_head window ;
	char window_name[64] ;
	int offset ;
	extern int stash_away() ;
	char buff[128] ;

/* Initialize the GIS calls */
	G_gisinit(argv[0]) ;

/* Check command line */
	set_default_options() ;

	if (D_parse_command(argc, argv, variables, n_variables, stash_away))
	{
		fprintf(stderr,"Usage: %s %s\n", argv[0], USAGE) ;
		exit(-1) ;
	}

/* Make sure map is available */
	if (*map_name == 0)
	{
	    mapset = G_ask_cell_old ("", map_name);
	    if (mapset == NULL)
		exit(0);
	}
	else
	{
	    mapset = G_find_cell (map_name, "") ;
	    if (mapset == NULL)
	    {
		sprintf(buff,"Cell file [%s] not available", map_name);
		G_fatal_error(buff) ;
	    }
	}

/* Read in the map window associated with graphics window */
	G_get_window(&window) ;

	if (G_set_window(&window) == -1) 
		G_fatal_error("Current window not settable") ;

	R_open_driver();

	if (D_get_cur_wind(window_name))
		G_fatal_error("No current graphics window (Run Dscreen)") ;

	if (D_set_cur_wind(window_name))
		G_fatal_error("Current graphics window not available") ;

/* Get color offset value for current graphics window and pass to driver */
	D_offset_is(&offset) ;
	R_color_offset(offset) ;

	get_map_info() ;

	R_close_driver();
}
