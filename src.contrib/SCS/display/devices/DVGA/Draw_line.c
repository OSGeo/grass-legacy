/* Function: draw_line		P.W. Carlson		12/88	*/

#include "vio_driver.h"

draw_line(x1, y1, x2, y2)
int x1, y1, x2, y2;
{
    if (y1 == y2 && x2 > x1)
    {	/* pass data to device driver for hor. line */
	args.arg1 = x1;
    	args.arg2 = x2;
    	args.arg3 = y1;
    	ioctl(viofd, VIO_HORLINE, &args);
    }
    else
    {	/* pass data to device driver for a move */
	args.arg1 = x1;
    	args.arg2 = y1;
    	ioctl(viofd, VIO_MOVE, &args);

	/* pass data to device driver for a draw */
    	cur_x = args.arg1 = x2;
    	cur_y = args.arg2 = y2;
    	ioctl(viofd, VIO_DRAW, &args);
    }
}
