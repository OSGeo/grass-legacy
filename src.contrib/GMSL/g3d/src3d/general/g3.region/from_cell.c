#include "glob.h"
#include "G3d.h"
from_cellhd()
{
    char name[30];
    char *mapset;
    struct Cell_head window2d;
    G3D_Region window;

    mapset = G_ask_cell_old ("", name);
    if (!mapset) return 1;

    if (G_get_cellhd (name, mapset, &window2d) < 0)
	fprintf (stderr, "header file for [%s in %s] invalid. can't select. ",
		name, mapset);
    else
    {
      G3d_incorporate2dRegion (&window2d, &window);
      window.top = 0;
      window.bottom= 1;
      window.tb_res = 1;
      G3d_adjustRegionRes (&window);
      if(!edit_window(&window)) return 1;
      set_window (&window, name);
    }
    return 0;
}
