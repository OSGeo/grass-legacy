/* Function: Polyline_rel		P.W. Carlson	1/90		*/

#include <sys/types.h>
#include <sys/at_ansi.h>
#include <sys/kd.h>
#include "vdc600.h"

Polyline_rel(xarray, yarray, number)
int *xarray, *yarray;
int number;
{
    register int *xptr, *yptr, n;

    /* get starting absolute coordinates */
    xptr = xarray;
    yptr = yarray;
    *xptr = current.x + *xptr;
    *yptr = current.y + *yptr;
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
