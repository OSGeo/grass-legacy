#include "pos.h"
#include "graph.h"

R_move_abs(x,y)
	int x, y ;
{
	int z ;
	_send_ident(MOVE_ABS) ;
	z = x ;
	_send_int(&z) ;
	z = y ;
	_send_int(&z) ;

	R__curx = x;
	R__cury = y;
}
