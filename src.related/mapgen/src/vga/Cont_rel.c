/* Function: Cont_rel		P.W. Carlson		12/88	*/

#include "vio_driver.h"

Cont_rel(delta_x, delta_y)
int delta_x, delta_y;
{
    /* update the current coordinates */
    cur_x += delta_x;
    cur_y += delta_y;

    /* pass the data to the device driver */
    args.arg1 = cur_x;
    args.arg2 = cur_y;
    ioctl(viofd, VIO_DRAW, &args);
}
