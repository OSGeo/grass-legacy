/* Function: Polydots_abs		P.W. Carlson	1/90	*/

#include <sys/types.h>
#include <sys/at_ansi.h>
#include <sys/kd.h>
#include "vdc600.h"

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
    {   setpixel(*xptr, *yptr);
	xptr++;
	yptr++;
    }
}
