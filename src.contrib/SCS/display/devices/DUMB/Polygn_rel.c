/* %W%   %G% */
#include "driver.h"
#include <stdio.h>
Polygon_rel(xarray, yarray, number)
	int *xarray, *yarray ;
	int number ;
{
	int first_x, first_y ;
	int num ;
	int *xptr, *yptr ;
	extern int cur_color;

	num = number ;
	xptr = xarray ;
	yptr = yarray ;

	put_chr('f');
	put_int(cur_color);
	put_int(num);
	first_x = cur_x + *xptr++ ;
	first_y = cur_y + *yptr++ ;
	put_int(first_x);
	put_int(first_y);

	while(--num)
	{
		cur_x += *xptr++ ;
		cur_y += *yptr++ ;
		put_int(cur_x);
		put_int(cur_y);
	}
}
