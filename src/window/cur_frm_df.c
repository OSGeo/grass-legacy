#include "glob.h"
cur_from_def()
{
    struct Cell_head window;

    G_get_default_window (&window);
    if(!edit_window(&window)) return 1;
    set_window (&window, "default window");
    return 0;
}
