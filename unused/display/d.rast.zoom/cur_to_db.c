#include "gis.h"
#include <string.h>
#include "glob.h"
int 
cur_to_db (struct Cell_head *window)
{
    char name[30], window_dir[30];
    char *mapset;

    strcpy(name,"lastwindow");
    strcpy(window_dir,"windows");
    G_copy(&cur_window,window,sizeof cur_window);
    mapset = G_mapset();
    /*
    mapset = G_ask_new_ext ("please enter a window name to save current window",
	name,window_dir,"window","with window values", lister);
    if (!mapset) return 1;
    */

    if (G__put_window (&cur_window, window_dir, name) < 0)
	fprintf (stderr, "** can't save window [%s].\n ", name);
    else
	fprintf (stderr, "current window saved as [%s].\n ", name);
    return 0;
}
