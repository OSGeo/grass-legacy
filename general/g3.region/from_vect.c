#include <stdio.h>
#include "gis.h"
#include "glob.h"
#include "Vect.h"
#include "G3d.h"
#include "local_proto.h"
int
from_vect()
{
    char name[1000];
    char *mapset;
    struct Cell_head window2d, cur_window;
    struct Map_info Map;
    BOUND_BOX box;
    char *G_align_window();
    G3D_Region window;

    G3d_getWindow (&window);
    G3d_extract2dRegion (&window, &cur_window);
    G_copy (&window2d, &cur_window, sizeof (window2d));

    mapset = G_ask_vector_old ("", name);
    if (!mapset) return 1;

    Vect_set_open_level(2);
    Vect_set_fatal_error ( GV_FATAL_RETURN );

    if (2 > Vect_open_old_head (&Map, name, mapset))
	fprintf (stderr, "can't read vector file [%s in %s]. ",
		name, mapset);
    else
    {
	Vect_get_map_box (&Map, &box );
	window2d.north  = box.N;
	window2d.south  = box.S;
	window2d.west   = box.W;
	window2d.east   = box.E;
	Vect_close (&Map);

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
	
	window.top    = box.T;
	window.bottom = box.B;
	window.tb_res = 1;
	if(window.top <= window.bottom)
	{
	      window.top = window.bottom + 1;
        }

	G3d_adjustRegionRes (&window);
	if(!edit_window(&window)) return 1;
	set_window (&window, name);
    }
    return 0;
}

