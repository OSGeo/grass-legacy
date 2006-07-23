#include <grass/gis.h>

#include <grass/raster.h>
#include <grass/graphics.h>
#include "transport.h"

int R_get_location_with_box(int cx, int cy, int *wx, int *wy, int *button)
{
	return trans->get_location_with_box(cx, cy, wx, wy, button);
}

int R_get_location_with_box_old(int cx, int cy, int *wx, int *wy, int *button)
{
	return trans->get_location_with_box_old(cx, cy, wx, wy, button);
}

int R_get_location_with_line(int cx, int cy, int *wx, int *wy, int *button)
{
	return trans->get_location_with_line(cx, cy, wx, wy, button);
}

int R_get_location_with_line_old(int cx, int cy, int *wx, int *wy, int *button)
{
	return trans->get_location_with_line_old(cx, cy, wx, wy, button);
}

int R_get_location_with_pointer(int *wx, int *wy, int *button)
{
	return trans->get_location_with_pointer(wx, wy, button);
}

int R_get_location_with_pointer_old(int *wx, int *wy, int *button)
{
	return trans->get_location_with_pointer_old(wx, wy, button);
}

