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
    struct Cell_head window ;
    char window_name[64] ;
    int offset ;
    char buff[128] ;
    struct Option *map;
    char *mapset;

/* Initialize the GIS calls */
    G_gisinit(argv[0]) ;
    R_open_driver();
    D_get_cell_name (buff) ;
    R_close_driver();

    map = G_define_option();
    map->key = "map";
    map->type = TYPE_STRING;
    map->answer = buff;
    map->required = NO;
    map->gisprompt = "old,cell,raster" ;
    map->description = "Name of raster map";

    if (G_parser(argc, argv))
	exit(1);

/* Make sure map is available */
    mapset = G_find_cell2 (map->answer, "") ;
    if (mapset == NULL)
    {
	sprintf(buff,"Raster file [%s] not available", map->answer);
	G_fatal_error(buff) ;
    }

/* connect to the driver */
    R_open_driver();

/* Read in the map region associated with graphics window */
    G_get_window(&window) ;

    if (G_set_window(&window) == -1) 
        G_fatal_error("Current region not settable") ;

    if (D_get_cur_wind(window_name))
        G_fatal_error("No current graphics window (Run d.screen)") ;

    if (D_set_cur_wind(window_name))
        G_fatal_error("Current graphics window not available") ;

/* Get color offset value for current graphics window and pass to driver */
    D_offset_is(&offset) ;
    R_color_offset(offset) ;

    get_map_info(map->answer, mapset) ;

    R_close_driver();
    exit(0);
}
