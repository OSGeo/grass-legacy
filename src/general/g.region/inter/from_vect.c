#include "glob.h"
#include "Vect.h"
from_vect()
{
    char name[30];
    char *mapset;
    struct Cell_head window;
    struct Map_info Map;

    G_copy (&window, &cur_window, sizeof (window));

    mapset = G_ask_vector_old ("", name);
    if (!mapset) return 1;

    Vect_set_open_level(1); /* don't need level 2 for vector region info */

    if (1 != Vect_open_old (&Map, name, mapset))
	fprintf (stderr, "can't read vector file [%s in %s]. ",
		name, mapset);
    else
    {
	window.north = Map.head.N;
	window.south = Map.head.S;
	window.west  = Map.head.W;
	window.east  = Map.head.E;
	G_align_window (&window, &cur_window);

	if(!edit_window (&window)) return 1;
	set_window (&window, name);
    }
    return 0;
}
