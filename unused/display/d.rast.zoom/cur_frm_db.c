#include "gis.h"
#include <string.h>

int cur_from_db (struct Cell_head *window, char *rast_name, char *rast_mapset)
{
    char name[30], window_dir[30];
    char *mapset;
    char command[200];
    char *G__get_window();

    strcpy(name,"lastwindow");
    strcpy(window_dir,"windows");
    mapset = G_mapset();

    if (G__get_window (window, window_dir, name, mapset) !=NULL)
	{
	  sprintf(command, "g.region rast=%s@%s", rast_name, rast_mapset);
	  G_system(command);
	  G_get_set_window(window);
	}
    else
	G_system("g.remove region=lastwindow");

    return 0;
}
