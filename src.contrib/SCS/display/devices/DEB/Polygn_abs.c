/* %W%   %G% */
#include "driver.h"
Polygon_abs(xarray, yarray, number)
	int xarray[], yarray[] ;
	int number ;
{
	int x, y ;
	int num ;
	int *xptr, *yptr ;
	extern int cur_color;

	put_chr('f');
	put_int(cur_color);
	put_int(number);
	for (num = 0; num < number; num++)
	{	put_int(xarray[num]);
		put_int(yarray[num]);
	}

	cur_x = xarray[number-1] ;
	cur_y = yarray[number-1] ;
}
