/* %W%   %G% */
#include "driver.h"
Polydots_rel(xarray, yarray, number)
	int *xarray, *yarray ;
	int number ;
{
	register int i ;
	register int *xptr, *yptr ;
	xptr = xarray ;
	yptr = yarray ;
	for(i=0; i<number; i++)
	{
		cur_x += *xptr ;
		cur_y += *yptr ;
		Move_abs(cur_x,cur_y);
		Cont_abs(cur_x+1,cur_y);
		xptr++ ;
		yptr++ ;
	}
}
