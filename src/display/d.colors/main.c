#include "gis.h"
#include "display.h"
#include "raster.h"
#include "colors.h"

/*
 *   d.colors
 *
 *   Usage:  d.colors raster=name
 *
 */

int 
main (int argc, char **argv)
{
    char name[128] ;
    struct Option *map;
    char *mapset;
    char buff[500];

/* Initialize the GIS calls */
    G_gisinit(argv[0]) ;
    if (R_open_driver() != 0)
	G_fatal_error ("No graphics device selected");

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

    if(G_raster_map_is_fp(map->answer, mapset))
    {
        sprintf(buff,"Raster file [%s] is floating point! \nd.colors only works with integer maps", map->answer);
        G_fatal_error(buff) ;
    }

/* connect to the driver */
    if (R_open_driver() != 0)
	G_fatal_error ("No graphics device selected");

/* Read in the map region associated with graphics window */
    D_setup(0);

    get_map_info(map->answer, mapset) ;

    R_close_driver();
    exit(0);
}
