/* Function: reset_color	P.W. Carlson		12/88	*/

#include "vio_driver.h"

reset_color(position, red, green, blue)
int position;
unsigned char red, green, blue;
{
    /* pass the data to the device driver */
    args.arg1 = position;
    args.arg2 = red / 4;
    args.arg3 = green / 4;
    args.arg4 = blue / 4;
    ioctl(viofd, VIO_PUTPAL, &args);
}
