/* Function: Polygon_rel	P.W. Carlson		12/88	*/

#include "vio_driver.h"

Polygon_rel(xarray, yarray, number)
int *xarray, *yarray;
int number;
{
    register int n;
    int *xptr, *yptr;

    /* make first coords. absolute */
    xptr = xarray;
    yptr = yarray;
    *xptr = cur_x + *xptr;
    *yptr = cur_y + *yptr;
    xptr++;
    yptr++;

    /* make remaining coords. absolute */
    for (n = 1; n < number; n++)
    {   *xptr = *(xptr-1) + *xptr;
	*yptr = *(yptr-1) + *yptr;
	xptr++;
	yptr++;
    }

    /* call Polygon_abs */
    Polygon_abs(xarray, yarray, number);
}
