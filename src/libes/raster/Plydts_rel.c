
#include "graph.h"

R_polydots_rel(xarray, yarray, number)
	int *xarray, *yarray ;
	int number ;
{
	int n ;
	_send_ident(POLYDOTS_REL) ;
	n = number ;
	_send_int(&n) ;
	_send_int_array(number,xarray) ;
	_send_int_array(number,yarray) ;
}
