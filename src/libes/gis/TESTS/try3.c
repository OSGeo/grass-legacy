#include "gis.h"
main()
{
    struct Cell_head cellhd;
    char name[30], *mapset;
    G_gisinit ("");

    mapset = G_ask_cell_old ("", name);
    if (mapset == NULL) exit(0);
    fprintf (stdout,"Cell header for %s in %s\n", name, mapset);
    if (G_get_cellhd (name, mapset, &cellhd) < 0)
    {
	fprintf (stdout,"OOPS\n");
	exit(1);
    }
    fprintf (stdout,"north: %lf\n", cellhd.north);
    fprintf (stdout,"south: %lf\n", cellhd.south);
    fprintf (stdout,"east:  %lf\n", cellhd.east);
    fprintf (stdout,"west:  %lf\n", cellhd.west);
}
