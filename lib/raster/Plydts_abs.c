#include "raster.h"
#include "graph.h"

int R_polydots_abs(int *xarray,int *yarray ,int number )
{
	int n ;
	_send_ident(POLYDOTS_ABS) ;
	n = number ;
	_send_int(&n) ;
	_send_int_array(number,xarray) ;
	_send_int_array(number,yarray) ;

	return 0;
}
