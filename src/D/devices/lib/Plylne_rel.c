/* %W%  %G% */
#include "driver.h"
Polyline_rel(xarray, yarray, number)
	int *xarray, *yarray ;
	int number ;
{
	int i ;

	Move_rel(xarray[0], yarray[0]) ;

	for (i=1; i<number; i++)
		Cont_rel(xarray[i], yarray[i]) ;
}
