#include "glob.h"
cur_to_db()
{
    char name[30];
    char *mapset;

    mapset = G_ask_new_ext ("please enter a window name to save current window",
	name,window_dir,"window","with window values", lister);
    if (!mapset) return 1;

    if (G__put_window (&cur_window, window_dir, name) < 0)
	fprintf (stderr, "** can't save window [%s]. ", name);
    else
	fprintf (stderr, "current window saved as [%s]. ", name);
    return 0;
}
