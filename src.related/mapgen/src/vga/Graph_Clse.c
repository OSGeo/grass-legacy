/* Function: Graph_close	P.W. Carlson		12/88	*/

#include "vio_driver.h"

Graph_Close()
{
    register int n;
    register unsigned char *ptr;

    /* fill the raster buffer with color 0 */
    ptr = raster_buff;
    for (n = 0; n < 1024; n++) *ptr++ = 0;

    /* erase the graphics screen */
    args.ptr = raster_buff;
    ioctl(viofd, VIO_ERASE, &args);

    /* return to text mode */
    ioctl(viofd, VIO_TEXTMODE, &args);

    /* restore the cursor location */
    ioctl(viofd, VIO_PUTCURS, &args);

    /* close the Orchid VGA device driver */
    close(viofd);

    /* close the Logitech Bus Mouse device driver */
    close(mousfd);
}
