/*
 * draw a line between two given points in the current color.
 *
 * Called by:
 *     Cont_abs() in ../lib/Cont_abs.c
 */

#include "pixrect/pr_line.h"
#include "graphics.h"

draw_line(cur_x, cur_y, x, y)
{
        Pr_brush brush;

        brush.width = 1;

        pw_vector(pixwin, cur_x, cur_y, x, y,
                Op, 1);
/*
printf("drawing line (%d,%d) - (%d,%d)\n", cur_x, cur_y, x, y) ;
*/
}
