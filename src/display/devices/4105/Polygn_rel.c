#include <stdio.h>
Polygon_rel(xarray, yarray, number)
	int *xarray, *yarray ;
	int number ;
{
	extern int current_x_pos ;
	extern int current_y_pos ;
	int n ;

	xarray[0] += current_x_pos ;
	yarray[0] += current_y_pos ;

	for (n=1; n<number; n++)
	{
		xarray[n] += xarray[n-1] ;
		yarray[n] += yarray[n-1] ;
	}
	Polygon_abs(xarray, yarray, number) ;
}
