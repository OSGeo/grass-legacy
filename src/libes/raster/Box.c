
#include "graph.h"
#include <stdio.h>

R_box_abs(x1,y1,x2,y2)
	int x1, y1, x2, y2 ;
{
	int z ;
	_send_ident(BOX_ABS) ;
	z = x1 ;
	_send_int(&z) ;
	z = y1 ;
	_send_int(&z) ;
	z = x2;
	_send_int(&z) ;
	z = y2;
	_send_int(&z) ;
}

R_box_rel(x,y)
	int x, y ;
{
	int z ;
	_send_ident(BOX_REL) ;
	z = x ;
	_send_int(&z) ;
	z = y ;
	_send_int(&z) ;
}
