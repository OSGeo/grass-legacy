/* Function: Polydots_rel		P.W. Carlson	12/88	*/

#include "vio_driver.h"

Polydots_rel(xarray, yarray, number)
int *xarray, *yarray;
int number;
{
    register int i;
    register int *xptr, *yptr;

    /* loop thru the arays */
    xptr = xarray;
    yptr = yarray;
    for (i = 0; i < number; i++)
    {   
	/* convert coordinates to absolute */
	cur_x += *xptr++;
	cur_y += *yptr++;

    	/* pass the data to the device driver */
	args.arg1 = cur_x;
        args.arg2 = cur_y;
	ioctl(viofd, VIO_SETPIX, &args);
    }
}
