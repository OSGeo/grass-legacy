
#include <string.h>
#include "gis.h"

int 
main (int argc, char *argv[])
{
    int stat;
    char name[40];
    char *mapset;
    struct Colors pcolr;

    G_gisinit(argv[0]) ;
    if (argc < 2)
    {
	mapset = G_ask_cell_old ("Which raster file needs a color table", name);
	if (mapset == NULL) exit(0);
    }
    else
    {
	strcpy (name, argv[1]);
	mapset = G_find_cell2 (name, "");
	if (mapset == NULL)
	{
	    fprintf (stderr, "%s - raster file not found\n", name);
	    exit(1);
	}
    }
    if((stat = G_make_colors (name, mapset, &pcolr)) >= 0)
    {
	if (stat > 0)
	    stat = G_write_colors (name, mapset, &pcolr);
	if (stat >= 0)
	    fprintf (stdout,"Color table for [%s] updated\n", name);
    }
    exit(0);
}
