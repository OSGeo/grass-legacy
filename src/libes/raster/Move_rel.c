#include "raster.h"
#include "graph.h"

int R_move_rel(int x,int y )
{
	int z ;
	_send_ident(MOVE_REL) ;
	z = x ;
	_send_int(&z) ;
	z = y ;
	_send_int(&z) ;

	return 0;
}
