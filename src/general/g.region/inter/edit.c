#include "gis.h"
#include "edit.h"

int 
edit_window (struct Cell_head *window)
{
    struct Cell_head temp_window;

    G_copy (&temp_window, window, sizeof(struct Cell_head));
    if(E_edit_cellhd (&temp_window, 0) == 0)
    {
	G_copy (window, &temp_window, sizeof(struct Cell_head));
	return 1;
    }
    return 0;
}
