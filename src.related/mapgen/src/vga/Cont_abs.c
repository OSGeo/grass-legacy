/* Function: Cont_abs		P.W. Carlson		12/88	*/

#include "vio_driver.h"

Cont_abs(x,y)
int x, y;
{
    /* set the current coordinates */
    cur_x = args.arg1 = x;
    cur_y = args.arg2 = y;

    /* pass the data to the device driver */
    ioctl(viofd, VIO_DRAW, &args);
}
