#include "glob.h"
#include "G3d.h"
#include "local_proto.h"
int
cur_from_db()
{
    char name[30];
    char *mapset;
    struct Cell_head window2d;
    G3D_Region window;
    char *err;
    char *G__get_window();

    mapset = G_ask_old_ext ("Select a region",
	name,window_dir,"region","with region values", lister2d);
    if (!mapset) return 1;

    if ((err = G__get_window (&window2d, window_dir, name, mapset)))
	fprintf (stderr, "region [%s] in mapset [%s] %s. can't select.\n",
		name, mapset, err);
    else
    {
      G3d_incorporate2dRegion (&window2d, &window);
      window.top = 0; 
      window.bottom = 1;
      window.tb_res = 1;
      G3d_adjustRegionRes (&window);
      if(!edit_window(&window)) return 1;
      set_window (&window, name);
    }
    return 0;
}
