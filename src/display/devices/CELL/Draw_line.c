
/*
 * draw a line between two given points in the current color.
 *
 * Called by:
 *     Cont_abs() in ../lib/Cont_abs.c
 */

#include "cell.h"

draw_line(cur_x, cur_y, x, y)
{
/*DEBUG fprintf (stderr, "DRAW_LINE (%d,%d) - (%d,%d)\n", cur_x, cur_y, x, y);*/
    bres_line (cur_x, cur_y, x, y);
}
