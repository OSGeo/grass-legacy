
/*
 * draw a line between two given points in the current color.
 *
 * Called by:
 *     Cont_abs() in ../lib/Cont_abs.c
 */

#include <gd.h>
#include "png.h"

draw_line(cur_x, cur_y, x, y)
	int cur_x, cur_y, x, y;
{

	gdImageLine(im, cur_x, cur_y, x, y, currentColor);
}
