
#include "graph.h"

R_set_window(t, b, l, r)
	int t, b, l, r ;
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
}
