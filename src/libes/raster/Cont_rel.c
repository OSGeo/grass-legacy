#include "pos.h"
#include "graph.h"

R_cont_rel(x,y)
	int x, y ;
{
	int z ;
	_send_ident(CONT_REL) ;
	z = x ;
	_send_int(&z) ;
	z = y ;
	_send_int(&z) ;

	R__curx += x;
	R__cury += y;
}
