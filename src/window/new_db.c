#include "glob.h"
new_db()
{
    struct Cell_head window ;
    char name[30];

    if (!G_ask_new_ext("enter name of window to be created",
	name, window_dir, "window", "with window values", lister))
	    return 1;

    G_get_default_window (&window);
    if(!edit_window (&window)) return 1;
    if(G__put_window (&window, window_dir, name) < 0)
    {
	fprintf (stderr, "** can't write window [%s]. ", name) ;
	return 0;
    }
    fprintf (stderr, "window [%s] created.\n", name) ;
    return make_current (&window, name);
}
