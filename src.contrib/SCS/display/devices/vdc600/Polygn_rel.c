/* Function: Polygon_rel	P.W. Carlson		1/90	*/

#include <sys/types.h>
#include <sys/at_ansi.h>
#include <sys/kd.h>
#include "vdc600.h"

Polygon_rel(xarray, yarray, number)
int *xarray, *yarray;
int number;
{
    register int n;
    int *xptr, *yptr;

    /* make first coords. absolute */
    xptr = xarray;
    yptr = yarray;
    *xptr = current.x + *xptr;
    *yptr = current.y + *yptr;
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
