
#include <string.h>
#include "gis.h"

int 
main (int argc, char *argv[])
{
    char name[40];
    char *mapset;
    struct History hist;

    G_gisinit(argv[0]) ;
    if (argc < 2)
    {
	mapset = G_ask_cell_in_mapset ("Which raster file needs an updated history", name);
	if (mapset == NULL) exit(0);
    }
    else
    {
	strcpy (name, argv[1]);
	mapset = G_find_cell2 (name, G_mapset());
	if (mapset == NULL)
	{
	    fprintf (stderr, "%s - raster file not found\n", argv[1]);
	    exit(1);
	}
    }
    G_read_history (name, mapset, &hist);
    if(G_edit_history (&hist) > 0 && G_write_history (name, &hist) >= 0)
	fprintf (stdout,"History file for [%s] updated\n", name);
    else
	fprintf (stdout,"History file for [%s] not updated\n", name);
    exit(0);
}
