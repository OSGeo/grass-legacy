#include "raster.h"
#include "graph.h"

int R_set_window(int t,int b,int l,int r )
{
	int i ;
	_send_ident(SET_WINDOW) ;
	i = t ;
	_send_int(&i) ;
	i = b ;
	_send_int(&i) ;
	i = l ;
	_send_int(&i) ;
	i = r ;
	_send_int(&i) ;

	return 0;
}
