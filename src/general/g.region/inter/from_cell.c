#include "glob.h"
#include "local_proto.h"
int 
from_cellhd (void)
{
    char name[30];
    char *mapset;
    struct Cell_head window;

    mapset = G_ask_cell_old ("", name);
    if (!mapset) return 1;

    if (G_get_cellhd (name, mapset, &window) < 0)
	fprintf (stderr, "header file for [%s in %s] invalid. can't select. ",
		name, mapset);
    else
    {
	if(!edit_window (&window)) return 1;
	set_window (&window, name);
    }
    return 0;
}
