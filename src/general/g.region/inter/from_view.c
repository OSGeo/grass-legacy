
#include "glob.h"	
	
from_view()
{
	char name[30], msg[128];
	char *mapset;
	struct G_3dview v;
	struct Cell_head window;
	FILE *fp;
	int ret;
	
	mapset = G_ask_old("Enter the name of an existing 3d.view file", name, "3d.view","3D view parameters");
	if (!mapset)
	{
		sprintf (msg, "3dview file <%s> not found", name);
		G_warning(msg);
		return(1);
	}

	G_3dview_warning(0); /* suppress boundary mismatch warning */

	G_copy (&window, &cur_window, sizeof(window));

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

	 
	window.north = v.vwin.north;
	window.south = v.vwin.south;
	window.west  = v.vwin.west;
	window.east  = v.vwin.east;

	window.rows = v.vwin.rows;
	window.cols = v.vwin.cols;
	window.ns_res = v.vwin.ns_res;
	window.ew_res = v.vwin.ew_res;

	if(!edit_window (&window)) return 1;
	set_window (&window, name);

	return(0);

}

