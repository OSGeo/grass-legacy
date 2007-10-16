#include "cairodriver.h"

/* current drawing position on the Cairo mask */
static int current_pos_x = -1;
static int current_pos_y = -1;

static int current_drawing_op = OP_NONE;

/* TODO: better function names and comments */

/*
   Sets the current drawing operation to be performed on
   the current cairo mask (stroke, fill, ...). If it is
   different than the current one, the current drawing op
   is finished.
 */
void set_drawing_op(int drawing_op)
{
	if (current_drawing_op != OP_NONE && drawing_op != current_drawing_op)
	{
		G_debug(1, "Changing drawing op: %d", drawing_op);
		finish_drawing_op();
	}
	current_drawing_op = drawing_op;
}

/* Finishes (performs) the current cairo drawing operation */
void finish_drawing_op(void)
{
	if (current_drawing_op == OP_NONE)
		return;

	G_debug(1, "Finishing drawing op: %d", current_drawing_op);

	switch (current_drawing_op)
	{
	case OP_STROKE:
		cairo_stroke(cairo);
		break;
	case OP_FILL:
		cairo_fill(cairo);
		break;
	}
	modified = 1;
	reset_pos();
	current_drawing_op = OP_NONE;
}

/* Move the current cairo drawing position (lazy) */
void move_to(int x, int y)
{
	if (current_pos_x != x || current_pos_y != y)
	{
		G_debug(3, "Moving from: %d %d to: %d, %d", current_pos_x, current_pos_y, x, y);
		cairo_move_to(cairo, (double) x, (double) y);
		set_pos(x, y);
	}
}

/* Indicate that the current cairo drawing position has changed */
void set_pos(int x, int y)
{
	current_pos_x = x;
	current_pos_y = y;
}

/* Reset the current cairo drawing position */
void reset_pos(void)
{
	current_pos_x = current_pos_y = -1;
}
