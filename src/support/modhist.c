/* %W% %G% */

#include "gis.h"

main (argc, argv) char *argv[];
{
    char name[40];
    char *mapset;
    struct History hist;

    G_gisinit ("support");
    if (argc < 2)
    {
	mapset = G_ask_cell_in_mapset ("Which cell file needs an updated history", name);
	if (mapset == NULL) exit(0);
    }
    else
    {
	strcpy (name, argv[1]);
	mapset = G_find_cell2 (name, G_mapset());
	if (mapset == NULL)
	{
	    fprintf (stderr, "%s - cell file not found\n", argv[1]);
	    exit(1);
	}
    }
    G_read_history (name, mapset, &hist);
    if(G_edit_history (&hist) > 0 && G_write_history (name, &hist) >= 0)
	printf ("History file for [%s] updated\n", name);
    else
	printf ("History file for [%s] not updated\n", name);
    exit(0);
}
