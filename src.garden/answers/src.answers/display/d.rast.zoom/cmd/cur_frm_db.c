#include "gis.h"

struct Cell_head cur_from_db()
{
    char name[30], window_dir[30];
    struct Cell_head window;
    char *mapset;
    char *G_align_window();

    strcpy(name,"lastwindow");
    strcpy(window_dir,"windows");
    mapset = G_mapset();

    if (G__get_window (&window, window_dir, name, mapset) != NULL){
       G_get_default_window(&window);
	}
    else
	G_system("g.remove region=lastwindow");

    return (window);
}
