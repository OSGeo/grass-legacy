#include "gis.h"
#include "local_proto.h"

int make_window (struct Cell_head *window, int rotate)
{
    if (window->proj != PROJECTION_LL)
	rotate = 0;

    if (box)
	return make_window_box (window);
    else
	return make_window_center (window);
}
