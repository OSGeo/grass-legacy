/*
 * draw a line between two given points in the current color.
 *
 * Called by:
 *     Cont_abs() in ../lib/Cont_abs.c
 */

#include "cell.h"

int draw_line (int cur_x, int cur_y, int x, int y)
{
/*DEBUG fprintf (stderr, "DRAW_LINE (%d,%d) - (%d,%d)\n", cur_x, cur_y, x, y);*/
    bres_line (cur_x, cur_y, x, y);

    return 0;
}
