#include "glob.h"
#include "Vect.h"
#include "G3d.h"
from_vect()
{
    char name[30];
    char *mapset, *err;
    struct Cell_head window2d, cur_window;
    struct Map_info Map;
    char *G_align_window();
    G3D_Region window;

    G3d_getWindow (&window);
    G3d_extract2dRegion (&window, &cur_window);
    G_copy (&window2d, &cur_window, sizeof (window2d));

    mapset = G_ask_vector_old ("", name);
    if (!mapset) return 1;

    Vect_set_open_level(1); /* don't need level 2 for vector region info */

    if (1 != Vect_open_old (&Map, name, mapset))
	fprintf (stderr, "can't read vector file [%s in %s]. ",
		name, mapset);
    else
    {
	window2d.north = Map.head.N;
	window2d.south = Map.head.S;
	window2d.west  = Map.head.W;
	window2d.east  = Map.head.E;
	if(window2d.north == window2d.south)
	{
	      window2d.north = window2d.north + 0.5 * cur_window.ns_res;
	      window2d.south = window2d.south - 0.5 * cur_window.ns_res;
        }
	if(window2d.east == window2d.west)
	{
	      window2d.west = window2d.west - 0.5 * cur_window.ew_res;
	      window2d.east = window2d.east + 0.5 * cur_window.ew_res;
        }

	G_align_window (&window2d, &cur_window);

	G3d_incorporate2dRegion (&window2d, &window);
	window.top = 0; window.bottom= 1;
	window.tb_res = 1;
	G3d_adjustRegionRes (&window);
	if(!edit_window(&window)) return 1;
	set_window (&window, name);
    }
    return 0;
}

