/* Function: Move_rel		P.W. Carlson		5/89		*/

#include "ega_io.h"

Move_rel(x, y)
int x, y;
{
    /* update current coordinates */
    cur_x += x;
    cur_y += y;

    /* pass data to device driver */
    args.arg1 = cur_x;
    args.arg2 = cur_y;
    ioctl(egafd, EGA_MOVE, &args);
}
