#include "glob.h"
from_cellhd()
{
    char name[30];
    char *mapset;
    struct Cell_head window;

    mapset = G_ask_cell_old ("select a cell header to become the current window", name);
    if (!mapset) return 1;

    if (G_get_cellhd (name, mapset, &window) < 0)
	fprintf (stderr, "cell header for [%s] in mapset [%s] invalid. can't select. ",
		name, mapset);
    else
    {
	if(!edit_window (&window)) return 1;
	set_window (&window, name);
    }
    return 0;
}
