#include "gis.h"
#include "glob.h"	
#include "G3d.h"
#include "local_proto.h"

int
from_view()
{
	char name[30], msg[128];
	char *mapset;
	struct G_3dview v;
	struct Cell_head window2d, cur_window;
	FILE *fp;
	int ret;
        G3D_Region window;
	
	mapset = G_ask_old("Enter the name of an existing 3d.view file", name, "3d.view","3D view parameters");
	if (!mapset)
	{
		sprintf (msg, "3dview file <%s> not found", name);
		G_warning(msg);
		return(1);
	}

	G_3dview_warning(0); /* suppress boundary mismatch warning */

	G3d_getWindow (&window);
	G3d_extract2dRegion (&window, &cur_window);
	G_copy (&window2d, &cur_window, sizeof(window2d));

	if(0 > (ret = G_get_3dview(name, mapset, &v))){
	    sprintf (msg, "can't read 3dview file <%s> in <%s>", name, mapset);
	    G_warning(msg);
	    return(1);
	}
	if (ret == 0){
	    sprintf (msg, "Old 3dview file. Region not found in <%s> in <%s>", name, mapset);
	    G_warning(msg);
	    return(1);
	}

	 
	window2d.north = v.vwin.north;
	window2d.south = v.vwin.south;
	window2d.west  = v.vwin.west;
	window2d.east  = v.vwin.east;

	window2d.rows = v.vwin.rows;
	window2d.cols = v.vwin.cols;
	window2d.ns_res = v.vwin.ns_res;
	window2d.ew_res = v.vwin.ew_res;

      G3d_incorporate2dRegion (&window2d, &window);
      window.top = window.bottom= 0;
      window.depths = 1;
      G3d_adjustRegionRes (&window);
      if(!edit_window(&window)) return 1;
      set_window (&window, name);
      return(0);

}

