/* Function: color		P.W. Carlson		12/88	*/

#include "vio_driver.h"

color(number)
int number;
{
    /* set the current color */
    cur_color = (unsigned char)number;

    /* pass the data to the device driver */
    args.arg1 = number;
    ioctl(viofd, VIO_SETCOLOR, &args);
}
