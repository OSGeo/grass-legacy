#include <string.h>
#include "gis.h"
#include "local_proto.h"

int main (int argc, char *argv[])
{
    int i;
    char name[40], *mapset;

    G_gisinit (argv[0]);

    if (argc > 1)
    {
	for (i = 1; i < argc; i++)
	{
	    strcpy (name, argv[i]);
	    if (mapset = G_find_cell2 (name, ""))
		grey_scale (name, mapset);
	    else
		fprintf (stderr, "%s not found\n", name);
	}
    }
    else
    {
	while(mapset = G_ask_cell_old ("which layer needs a grey scale?",name))
	    grey_scale (name, mapset);
    }

    return 0;
}
