/*  %W%  %G%  */

/*
 *   Dtitle
 *
 *   Print a title for a map to stdout
 */

#define USAGE	"[name=cell_file] [color=name] [size=num] [type=fancy]"

#include "gis.h"
#define MAIN
#include "options.h"

main(argc, argv)
	int argc ;
	char **argv ;
{
	char buff[128] ;
	char window_name[64] ;
	char *mapset ;
	struct Cell_head window ;
	struct Categories cats ;
	extern int stash_away() ;

/* Initialize the GIS calls */
	G_gisinit("Dtitle") ;

/* Check command line */
	set_default_options() ;

	if (D_parse_command(argc, argv, variables, n_variables, stash_away))
	{
		fprintf(stderr,"Usage: %s %s\n", argv[0], USAGE) ;
		exit(-1) ;
	}

	if (! strlen(map_name))
	{
		fprintf(stderr,"Usage: %s %s\n", argv[0], USAGE) ;
		exit(-1) ;
	}

/* Make sure map is available */
	mapset = G_find_cell (map_name, "") ;
	if (mapset == NULL)
	{
		sprintf(buff,"Cellfile [%s] not available", map_name);
		G_fatal_error(buff) ;
	}

	if (G_get_cellhd(map_name, mapset, &window) == -1)
	{
		sprintf(buff,"Cell head file for [%s] not available", map_name) ;
			G_fatal_error(buff) ;
	}

	if (G_read_cats(map_name, mapset, &cats) == -1)
	{
		sprintf(buff,"Category file for [%s] not available", map_name) ;
			G_fatal_error(buff) ;
	}

	if (type == NORMAL)
		normal(mapset, &window, &cats) ;
	else
		fancy(mapset, &window, &cats) ;
}
