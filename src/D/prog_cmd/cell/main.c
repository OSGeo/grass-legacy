/*
 *   Dcell
 *
 *   Usage:  Dcell cell_file
 *
 */

#define USAGE	"cell_file"

#define MAIN
#include "options.h"
#include "gis.h"

main(argc, argv)
	int argc ;
	char **argv ;
{
	char *mapset ;
	char buff[128] ;
	extern int stash_away() ;

/* Initialize the GIS calls */
	G_gisinit("Dcell") ;

/* Check command line */
	set_default_options() ;

	if (D_parse_command(argc, argv, variables, n_variables, stash_away))
	{
		fprintf(stderr,"Usage: %s %s\n", argv[0], USAGE) ;
		exit(-1) ;
	}
	if (*name == 0)
	{
		fprintf(stderr,"Usage: %s %s\n", argv[0], USAGE) ;
		exit(-1) ;
	}

/* Make sure map is available */
	mapset = G_find_cell2 (name, "") ;
	if (mapset == NULL)
	{
		sprintf(buff,"Cellfile [%s] not available", name);
		G_fatal_error(buff) ;
	}

	R_open_driver();

	Dcell(name, mapset, overlay) ;

	R_close_driver();
}
