#include "glob.h"
int 
cur_to_db (void)
{
    char name[30];
    char *mapset;

    mapset = G_ask_new_ext ("Enter a name for the current region",
	name,window_dir,"region","with region values", lister);
    if (!mapset) return 1;

    if (G__put_window (&cur_window, window_dir, name) < 0)
	fprintf (stderr, "** can't save region [%s]. ", name);
    else
	fprintf (stderr, "current region saved as [%s]. ", name);
    return 0;
}
