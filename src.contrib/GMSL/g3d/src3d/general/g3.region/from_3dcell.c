#include "glob.h"
#include "G3d.h"
from_3dcellhd()
{
    char name[30];
    char *mapset;
    G3D_Region window;

    mapset = G_ask_old ("", name, G3D_DIRECTORY, "3d raster");
    if (!mapset) return 1;

    if (G3d_readRegionMap (name, mapset, &window) == 0)
	fprintf (stderr, "header file for [%s in %s] invalid. can't select. ",
		name, mapset);
    else
    {
	if(!edit_window (&window)) return 1;
	set_window (&window, name);
    }
    return 0;
}
