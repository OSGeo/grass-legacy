/* Function: Polyline_abs	P.W. Carlson		5/89	*/

#include "ega_io.h"

Polyline_abs(xarray, yarray, number)
int *xarray, *yarray;
int number;
{
    int *xptr, *yptr;

    /* move to first point */
    xptr = xarray;
    yptr = yarray;
    args.arg1 = *xptr++;
    args.arg2 = *yptr++;
    ioctl(egafd, EGA_MOVE, &args);

    /* draw to remaining points */
    while (--number)
    {	cur_x = args.arg1 = *xptr++;
    	cur_y = args.arg2 = *yptr++;
    	ioctl(egafd, EGA_DITHDRAW, &args);
    }
}
