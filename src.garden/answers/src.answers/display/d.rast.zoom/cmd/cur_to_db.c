#include "gis.h"
#include "glob.h"
cur_to_db(window)
struct Cell_head window;
{
    char name[30], window_dir[30];
    char *mapset;

    strcpy(name,"lastwindow");
    strcpy(window_dir,"windows");
    G_copy(&cur_window,&window,sizeof cur_window);
    mapset = G_mapset();
    /*
    mapset = G_ask_new_ext ("please enter a window name to save current window",
	name,window_dir,"window","with window values", lister);
    if (!mapset) return 1;
    */

    if (G__put_window (&cur_window, window_dir, name) < 0)
	printf ("** can't save window [%s]. ", name);
    else
	printf ("current window saved as [%s]. ", name);
    return 0;
}
