/* Functions: Box_abs, Box_rel		P.W. Carlson 	5/89	*/

#include "ega_io.h"

Box_abs(left, top, right, bottom)
int left, top, right, bottom;
{
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

    /* pass data to device driver */
    args.arg1 = left;
    args.arg2 = top;
    args.arg3 = right;
    args.arg4 = bottom;
    ioctl(egafd, EGA_DITHBOX, &args);
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
