/* Function: Polyline_abs	P.W. Carlson		1/90	*/

#include <sys/types.h>
#include <sys/at_ansi.h>
#include <sys/kd.h>
#include "vdc600.h"

Polyline_abs(xarray, yarray, number)
int *xarray, *yarray;
int number;
{
    int *xptr, *yptr;

    /* move to first point */
    xptr = xarray;
    yptr = yarray;
    Move_abs(*xptr, *yptr);
/*-->  RLG, SCS */
/*  xptr++;
    yptr++; */

    /* draw to remaining points */
    while (--number)
    {	current.x = *xptr++;
    	current.y = *yptr++;
	Cont_abs(*xptr, *yptr);
    }
}
