/*
 * A polygon is drawn using the current color.  It has "number" verticies
 * which are found in the absolute coordinate pairs represented in the
 * "xarray" and "yarray" arrays.
 */

extern int SCREEN_BOTTOM ;
#include "sun.h" ;

Polygon_abs(xarray, yarray, number)
	int *xarray, *yarray ;
	int number ;
{
	float *x ;
	float *y ;
	float *yptr ;
	int num ;

	i_to_f_array(xarray, yarray, &x, &y, number) ;
/* flip array */
	for(num=number, yptr=y; num; num--, yptr++)
		*yptr = SCREEN_BOTTOM - *yptr ;

	move_abs_2((float)xarray[0], (float)(SCREEN_BOTTOM - yarray[0])) ;
	polygon_abs_2(x, y, number) ;
	sun_x = xarray[number-1] ;
	sun_y = yarray[number-1] ;
}
