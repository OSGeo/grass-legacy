/* Functions: Move_abs, Get_current_xy		P.W. Carlson	12/88	*/

#include "vio_driver.h"

Move_abs(x, y)
int x, y;
{
    /* set current coords. and pass data to device driver */
    cur_x = args.arg1 = x;
    cur_y = args.arg2 = y;
    ioctl(viofd, VIO_MOVE, &args);
}


Get_current_xy(x, y)
int *x, *y;
{
    *x = cur_x;
    *y = cur_y;
}

