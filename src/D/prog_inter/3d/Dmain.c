/* @(#)Dmain.c	2.1   10/1/87 */
#define MAIN
#include "gis.h"
#include "options.h"
#define USAGE	"lots of stuff"

main(argc, argv)  char *argv[] ;
{
	char *location        ;
	char *mapset         ;
	char *name[20]        ;
	char buffer[256]      ;
	extern int stash_away() ;
	char window_name[64] ;

	G_gisinit(argv[0]) ;

	G_get_window(&window) ;

/* Gather the 3d display parameters */
	set_default_options(&window) ;

	if (D_parse_command(argc, argv, variables, n_variables, stash_away))
	{
		usage(argv[0]) ;
		exit(-1) ;
	}

/* Final check for existence of files */
	mapset = G_find_cell (file, "") ;
	if (strlen(file_mapset) == 0)
	{
		sprintf(buffer, "Map: [%s] not found", file) ;
		G_fatal_error(buffer) ;
		exit(-1) ;
	}
	mapset = G_find_cell (elevfile, "") ;
	if (strlen(file_mapset) == 0)
	{
		sprintf(buffer, "Map: [%s] not found", elevfile) ;
		G_fatal_error(buffer) ;
		exit(-1) ;
	}

	if (check_options() )
		G_fatal_error("Inappropriate 3d request") ;

	if (0 > G_set_window(&window) )
		G_fatal_error("Inappropriate window resolution request") ;

/* Set up graphics */
	R_open_driver();

	if (D_get_cur_wind(window_name))
		G_fatal_error("No current graphics window") ;

	if (D_set_cur_wind(window_name))
		G_fatal_error("Current graphics window not available") ;

	establish_view(from_easting,from_northing,from_height,to_easting,to_northing,to_height,field) ;

	threed(0);

	R_close_driver() ;
}
