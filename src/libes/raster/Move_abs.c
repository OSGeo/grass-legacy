#include "raster.h"
#include "pos.h"
#include "graph.h"

int R_move_abs(int x,int y)
{
	int z ;
	_send_ident(MOVE_ABS) ;
	z = x ;
	_send_int(&z) ;
	z = y ;
	_send_int(&z) ;

	R__curx = x;
	R__cury = y;

	return 0;
}
