#include "gis.h"
main()
{
    struct Cell_head cellhd;
    char name[30], *mapset;
    G_gisinit ("");

    mapset = G_ask_cell_old ("", name);
    if (mapset == NULL) exit(0);
    printf ("Cell header for %s in %s\n", name, mapset);
    if (G_get_cellhd (name, mapset, &cellhd) < 0)
    {
	printf ("OOPS\n");
	exit(1);
    }
    printf ("north: %lf\n", cellhd.north);
    printf ("south: %lf\n", cellhd.south);
    printf ("east:  %lf\n", cellhd.east);
    printf ("west:  %lf\n", cellhd.west);
}
