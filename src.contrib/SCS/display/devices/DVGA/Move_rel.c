/* Function: Move_rel		P.W. Carlson		12/88		*/

#include "vio_driver.h"

Move_rel(x, y)
int x, y;
{
    /* update current coordinates */
    cur_x += x;
    cur_y += y;

    /* pass data to device driver */
    args.arg1 = cur_x;
    args.arg2 = cur_y;
    ioctl(viofd, VIO_MOVE, &args);
}
