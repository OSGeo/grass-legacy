#include "gis.h"

main(argc, argv)
    int argc ;
    char **argv ;
{
    char *mapset ;
    char *name ;
	char buffer[256];
	int head_ok;
	double E, N;
	struct Cell_head cellhd;
    struct Option *map;

/* Initialize the GIS calls */
    G_gisinit(argv[0]) ;

/* set up command line */
    map              = G_define_option();
    map->key         = "map";
    map->type        = TYPE_STRING;
    map->required    = YES;
    map->gisprompt   = "old,cell,raster" ;
    map->description = "Ortho map to be shifted";


    if (G_parser(argc, argv))
	exit(1);

    name = map->answer;

/* Make sure map is available */
    mapset = G_find_cell2 (name, "") ;
    if (mapset == NULL)
    {
        sprintf(buffer,"Raster map [%s] not available", name);
        G_fatal_error(buffer) ;
    }

    R_open_driver();
	D_setup(0);

	head_ok = G_get_cellhd (name, mapset, &cellhd) >= 0;
	if (!head_ok)
		{
		sprintf(buffer,"Header for [%s] not available\nRun r.support.\n", name);
		G_fatal_error(buffer) ;
		}
    get_shift(&E,&N);
	cellhd.north += N;
	cellhd.south += N;
	cellhd.east += E; 
	cellhd.west += E;
/* Write new header out */
	if (G_put_cellhd(name, &cellhd) == -1)
		{
		sprintf (buffer, "unable to write header for %s", name);
		G_fatal_error(buffer) ;
		}
	else
		printf ("header for [%s] updated\n", name);
    R_close_driver();
	exit(0) ;
}
