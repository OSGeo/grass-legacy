#include "glob.h"
#include "G3d.h"
cur_from_def()
{
    struct Cell_head window2d;
    G3D_Region window;

    G_get_default_window (&window2d);
    G3d_incorporate2dRegion (&window2d, &window);
    window.top = 0;
    window.bottom = 1;
    window.tb_res = 1;
    G3d_adjustRegionRes (&window);
    if(!edit_window(&window)) return 1;
    set_window (&window, "default region");
    return 0;
}
