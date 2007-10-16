#include "cairodriver.h"

void Cairo_draw_line(int x1, int y1, int x2, int y2)
{
	G_debug(3, "Cairo_draw_line: %d %d %d %d", x1, y1, x2, y2);

	/* TODO: support clipping */

	set_drawing_op(OP_STROKE);
	move_to(x1, y1);
	if (x1 == x2 && y1 == y2)
	{
		/* don't draw degenerate lines */
		G_debug(3, "Skipping zero-length line");
		return;
	}
	cairo_line_to(cairo, x2, y2);
	set_pos(x2, y2);
}
