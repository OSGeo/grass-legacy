/* %W%   %G% */
#include "driver.h"

Polyline_rel(xarray, yarray, number)
	int *xarray, *yarray ;
	int number ;
{
	int *xptr, *yptr ;
	int n ;

	xptr = xarray ;
	yptr = yarray ;

	*xptr = cur_x + *xptr ;
	*yptr = cur_y + *yptr ;

	xptr++ ;
	yptr++ ;

	for(n=1; n<number; n++)		/* n = 0 --> n = 1 */
	{
		*xptr = *(xptr-1) + *xptr ;
		*yptr = *(yptr-1) + *yptr ;
		xptr++ ;
		yptr++ ;
	}

	Polyline_abs(xarray, yarray, number) ;
}
