#include "glob.h"
#include "Vect.h"
from_vect()
{
    char name[30];
    char *mapset, *err;
    struct Cell_head window;
    struct Map_info Map;
    char *G_align_window();

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
	if(window.north == window.south)
	{
	      window.north = window.north + 0.5 * cur_window.ns_res;
	      window.south = window.south - 0.5 * cur_window.ns_res;
        }
	if(window.east == window.west)
	{
	      window.west = window.west - 0.5 * cur_window.ew_res;
	      window.east = window.east + 0.5 * cur_window.ew_res;
        }

	G_align_window (&window, &cur_window);

	if(!edit_window (&window)) return 1;
	set_window (&window, name);
    }
    return 0;
}

