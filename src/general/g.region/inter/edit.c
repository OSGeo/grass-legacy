#include "gis.h"
edit_window (window)
    struct Cell_head *window;
{
    struct Cell_head temp_window;

    G_copy (&temp_window, window, sizeof(struct Cell_head));
    if(G_edit_cellhd (&temp_window, 0) == 0)
    {
	G_copy (window, &temp_window, sizeof(struct Cell_head));
	return 1;
    }
    return 0;
}
