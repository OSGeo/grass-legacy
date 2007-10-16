#include "cairodriver.h"

void do_polygon(const int *xarray, const int *yarray, int count)
{
	int i;

	move_to(xarray[0], yarray[0]);

	for (i = 1; i < count; i++)
	{
		cairo_line_to(cairo, xarray[i], yarray[i]);
		set_pos(xarray[i], yarray[i]);
	}
	reset_pos();
}

void Cairo_Polygon_abs(const int *xarray, const int *yarray, int count)
{
	G_debug(3, "Cairo_Polygon_abs (%d points)", count);
	set_drawing_op(OP_FILL);
	do_polygon(xarray, yarray, count);
}

void Cairo_Polyline_abs(const int *xarray, const int *yarray, int count)
{
	G_debug(3, "Cairo_Polyline_abs (%d points)", count);
	set_drawing_op(OP_STROKE);
	do_polygon(xarray, yarray, count);
}
