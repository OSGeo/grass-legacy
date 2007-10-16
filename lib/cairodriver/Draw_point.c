#include "cairodriver.h"

#define POINTSIZE 1.0
#define HALFPOINTSIZE (0.5*POINTSIZE)

void Cairo_draw_point(int x, int y)
{
	G_debug(3, "Cairo_draw_point: %d %d", x, y);

	/* TODO: support clipping */

	/* draw a point in the form of a small rectangle */
	set_drawing_op(OP_FILL);
	cairo_rectangle(cairo, (double) x - HALFPOINTSIZE, (double) y - HALFPOINTSIZE, POINTSIZE, POINTSIZE);
	reset_pos();
}
