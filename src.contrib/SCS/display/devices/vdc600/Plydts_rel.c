/* Function: Polydots_rel		P.W. Carlson	1/90	*/

#include <sys/types.h>
#include <sys/at_ansi.h>
#include <sys/kd.h>
#include "vdc600.h"

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
	current.x += *xptr++;
	current.y += *yptr++;
	setpixel(current.x, current.y);
    }
}
