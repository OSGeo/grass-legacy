/* Function: Polydots_abs		P.W. Carlson	12/88	*/

#include "vio_driver.h"

Polydots_abs(xarray, yarray, number)
int *xarray, *yarray;
int number;
{
    register int i;
    register int *xptr, *yptr;

    /* loop thru the arrays */
    xptr = xarray;
    yptr = yarray;
    for (i = 0; i < number; i++)
    {   
    	/* pass the data to the device driver */
	args.arg1 = *xptr++;
        args.arg2 = *yptr++;
	ioctl(viofd, VIO_SETPIX, &args);
    }
}
