#include "raster.h"
#include "graph.h"

int R_cont_rel(int x,int y)
{
	int z ;
	_send_ident(CONT_REL) ;
	z = x ;
	_send_int(&z) ;
	z = y ;
	_send_int(&z) ;

	return 0;
}
