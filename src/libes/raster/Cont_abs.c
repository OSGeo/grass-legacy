#include "raster.h"
#include "graph.h"

int R_cont_abs(int x,int y)
{
	int z ;
	_send_ident(CONT_ABS) ;
	z = x ;
	_send_int(&z) ;
	z = y ;
	_send_int(&z) ;

	return 0;
}
