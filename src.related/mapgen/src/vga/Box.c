/* Functions: Box_abs, Box_rel		P.W. Carlson 	12/88	*/

#include "vio_driver.h"

Box_abs(left, top, right, bottom)
int left, top, right, bottom;
{
    register n;
    register unsigned char *ptr;
    int tmp;

    /* make swaps if necessary */
    if (left > right)
    { 	tmp = left; 
	left = right; 
	right = tmp; 
    }
    if (top > bottom)
    { 	tmp = top; 
	top = bottom; 
	bottom = tmp; 
    }

    /* get color into raster buffer */
    ptr = raster_buff;
    for (n = left; n <= right; n++) *ptr++ = cur_color;

    /* pass data to device driver */
    args.arg1 = left;
    args.arg2 = top;
    args.arg3 = right;
    args.arg4 = bottom;
    args.ptr  = raster_buff;
    ioctl(viofd, VIO_PUTRAST, &args);
}


Box_rel(left, top, right, bottom)
int left, top, right, bottom;
{
    /* convert to absolute coords. and call Box_abs */
    top += cur_y;
    bottom += cur_y;
    left += cur_x;
    right += cur_x;
    Box_abs(top, bottom, left, right);
}
