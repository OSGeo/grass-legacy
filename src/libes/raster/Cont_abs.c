#include "pos.h"
#include "graph.h"

R_cont_abs(x,y)
	int x, y ;
{
	int z ;
	_send_ident(CONT_ABS) ;
	z = x ;
	_send_int(&z) ;
	z = y ;
	_send_int(&z) ;

	R__curx = x;
	R__cury = y;
}
