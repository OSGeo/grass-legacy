#include "gis.h"
#include "G3d.h"
edit_window (window)
    G3D_Region *window;
{
    G3D_Region temp_window;

    G3d_regionCopy (&temp_window, window);

    if(edit_3dcellhd (&temp_window, 0) == 0)
    {
        G3d_regionCopy (window, &temp_window);
	return 1;
    }
    return 0;
}
