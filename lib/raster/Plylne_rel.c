#include "raster.h"
#include "graph.h"

int R_polyline_rel(int *xarray,int *yarray, int number )
{
	int n ;
	_send_ident(POLYLINE_REL) ;
	n = number ;
	_send_int(&n) ;
	_send_int_array(number,xarray) ;
	_send_int_array(number,yarray) ;

	return 0;
}
