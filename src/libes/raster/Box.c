#include "raster.h"
#include "graph.h"

int R_box_abs(int x1,int y1,int x2,int y2 )
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

	return 0;
}

int R_box_rel(int x,int y )
{
	int z ;
	_send_ident(BOX_REL) ;
	z = x ;
	_send_int(&z) ;
	z = y ;
	_send_int(&z) ;

	return 0;
}
