#include "gis.h"

/*
 *   d.colors
 *
 *   Usage:  d.colors raster=name
 *
 */

main(argc, argv)
    char **argv ;
{
    char name[128] ;
    struct Option *map;
    char *mapset;

/* Initialize the GIS calls */
    G_gisinit(argv[0]) ;
    R_open_driver();
    if(D_get_cell_name (name) < 0)
	*name = 0;
    R_close_driver();

    map = G_define_option();
    map->key = "map";
    map->type = TYPE_STRING;
    if (*name)
	map->answer = name;
    if (*name)
	map->required = NO;
    else
	map->required = YES;
    map->gisprompt = "old,cell,raster" ;
    map->description = "Name of raster map";

    if (G_parser(argc, argv))
	exit(1);

/* Make sure map is available */
    if (map->answer == NULL) exit(0);
    mapset = G_find_cell2 (map->answer, "") ;
    if (mapset == NULL)
    {
	char msg[256];
	sprintf(msg,"Raster file [%s] not available", map->answer);
	G_fatal_error(msg) ;
    }

/* connect to the driver */
    R_open_driver();

/* Read in the map region associated with graphics window */
    D_setup(0);

    get_map_info(map->answer, mapset) ;

    R_close_driver();
    exit(0);
}
