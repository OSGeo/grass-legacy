#include "gis.h"

main(argc, argv)
    int argc ;
    char **argv ;
{
    char *mapset ;
    char *name ;
    int overlay;
    struct Option *map;
    struct Flag *flag_o;

/* Initialize the GIS calls */
    G_gisinit(argv[0]) ;

/* set up command line */
    map              = G_define_option();
    map->key         = "map";
    map->type        = TYPE_STRING;
    map->required    = YES;
    map->gisprompt   = "old,cell,raster" ;
    map->description = "Raster map to be displayed";

    flag_o = G_define_flag();
    flag_o->key = 'o';
    flag_o->description = "Overlay (non-zero values only)";

    if (G_parser(argc, argv))
	exit(1);

    name = map->answer;
    overlay = flag_o->answer;

/* Make sure map is available */
    mapset = G_find_cell2 (name, "") ;
    if (mapset == NULL)
    {
		char buf[256];
        sprintf(buf,"Raster map [%s] not available", name);
        G_fatal_error(buf) ;
    }

    R_open_driver();

    Dcell(name, mapset, overlay) ;

    R_close_driver();

    exit(0);
}
