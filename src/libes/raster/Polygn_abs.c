
#include "graph.h"

R_polygon_abs(xarray, yarray, number)
	int *xarray, *yarray ;
	int number ;
{
	int n ;
	_send_ident(POLYGON_ABS) ;
	n = number ;
	_send_int(&n) ;
	_send_int_array(number,xarray) ;
	_send_int_array(number,yarray) ;
}
