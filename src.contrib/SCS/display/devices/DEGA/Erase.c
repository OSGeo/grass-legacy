/* Function: Erase		P.W. Carlson		12/88	*/

#include "ega_io.h"

Erase()
{
    register int n;
    register unsigned char *ptr;

    /* fill the raster buffer with color 0 */
    ptr = raster_buff;
    for (n = 0; n < 1024; n++) *ptr++ = 0;

    /* pass the data to the device driver */
    args.ptr1 = raster_buff;
    ioctl(egafd, EGA_ERASE, &args);
}
