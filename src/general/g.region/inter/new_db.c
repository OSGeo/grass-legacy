#include "glob.h"
#include "local_proto.h"
int 
new_db (void)
{
    struct Cell_head window ;
    char name[30];

    if (!G_ask_new_ext("Enter name for region to be created",
	name, window_dir, "region", "with region values", lister))
	    return 1;

    G_get_default_window (&window);
    if(!edit_window (&window)) return 1;
    if(G__put_window (&window, window_dir, name) < 0)
    {
	fprintf (stderr, "** can't write region [%s]. ", name) ;
	return 0;
    }
    fprintf (stderr, "region [%s] created.\n", name) ;
    return make_current (&window, name);
}
