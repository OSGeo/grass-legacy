#include "glob.h"
#include "local_proto.h"
int 
cur_from_def (void)
{
    struct Cell_head window;

    G_get_default_window (&window);
    if(!edit_window(&window)) return 1;
    set_window (&window, "default region");
    return 0;
}
