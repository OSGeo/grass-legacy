/* Function: Polyline		P.W. Carlson	5/89		*/

#include "ega_io.h"

Polyline_rel(xarray, yarray, number)
int *xarray, *yarray;
int number;
{
    register int *xptr, *yptr, n;

    /* get starting absolute coordinates */
    xptr = xarray;
    yptr = yarray;
    *xptr = cur_x + *xptr;
    *yptr = cur_y + *yptr;
    xptr++;
    yptr++;

    /* make the remaining coorinates absolute */
    for (n = 1; n < number; n++)
    {   *xptr = *(xptr-1) + *xptr;
	*yptr = *(yptr-1) + *yptr;
	xptr++;
	yptr++;
    }

    /* call Polyline_abs */
    Polyline_abs(xarray, yarray, number);
}
