/* Function: color		P.W. Carlson		5/89	*/

#include "ega_io.h"

color(number)
int number;
{
    static int no_dither[] = {
	0,1,1,1,1,   2,1,1,1,1,   2,2,1,1,1,   2,2,2,3,3,   2,2,3,3,3,
	4,5,5,5,5,   2,5,5,5,1,   2,2,3,3,3,   2,2,3,3,3,   2,2,3,3,3,
	4,5,5,5,5,   6,5,5,5,5,   6,6,5,5,5,   6,6,6,5,5,   6,6,6,3,7,
	4,5,5,5,5,   6,5,5,5,5,   6,6,5,5,5,   6,6,6,5,7,   6,6,6,7,7,
	4,4,5,5,5,   6,4,5,5,5,   6,6,5,5,5,   6,6,6,5,7,   6,6,6,7,7  };

    /* set the current color */
    cur_color = (unsigned char)number;

    /* pass the data to the device driver */
    args.arg1 = number;
    ioctl(egafd, EGA_SETDITH, &args);
    args.arg1 = no_dither[number];
    ioctl(egafd, EGA_SETCOLOR, &args);
}
