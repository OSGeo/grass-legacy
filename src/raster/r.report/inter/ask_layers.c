#include "global.h"
ask_layers()
{
    char *prompt;
    char name[256];
    char *mapset;

    G_clear_screen();
    printf ("You may run a report on a single raster map, or on many maps.\n");
    printf ("A single map will produce a simple tabulation report.\n");
    printf ("Multiple maps will produce a multi-level coincidence tabulation report.\n");

    layer = NULL;

    for (nlayers = 0;;nlayers++)
    {
	if (nlayers==0)
	    prompt="";
	else if (nlayers==1)
	{
	    prompt="Enter a second map, if you want a coincidence tabulation";
	    G_set_ask_return_msg("if you only want a simple report");
	}
	else
	{
	    prompt="Enter another map";
	    G_set_ask_return_msg("if you have selected all the maps for the report");
	}
	mapset = G_ask_cell_old (prompt, name);
	if (mapset == NULL) break;

	layer = (struct layer *) G_realloc (layer, (nlayers+1) * sizeof(struct layer));
	layer[nlayers].name = G_store(name);
	layer[nlayers].mapset = G_store(mapset);
    }
    if (nlayers == 0) exit(0);
}
