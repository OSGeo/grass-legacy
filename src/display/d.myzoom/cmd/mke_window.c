#include "gis.h"

make_window(window, rotate)
    struct Cell_head *window ;
{
    if (window->proj != PROJECTION_LL)
	rotate = 0;

    if (box)
	return make_window_box (window);
    else
	return make_window_center (window);
}
