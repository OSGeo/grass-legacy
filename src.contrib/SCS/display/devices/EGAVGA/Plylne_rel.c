/* Function: Polyline    	P.W. Carlson		April 1990	*/

#include "driver.h"

Polyline_rel(xarray, yarray, number)
int *xarray, *yarray, number;
{
    int *xptr, *yptr;
    int n;

    xptr = xarray;
    yptr = yarray;
    *xptr = cur_x + *xptr;
    *yptr = cur_y + *yptr;
    xptr++;
    yptr++;

    for(n = 1; n < number; n++)
    {	*xptr = *(xptr - 1) + *xptr;
    	*yptr = *(yptr - 1) + *yptr;
    	xptr++;
    	yptr++;
    }

    Polyline_abs(xarray, yarray, number);
}
