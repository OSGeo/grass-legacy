
#include "graph.h"

R_get_color_index(r, g, b)
	float r, g, b ;
{
	int i ;
	float f ;

	_send_ident(GET_COLOR_INDEX) ;
	f = r ; _send_float(&f) ;
	f = g ; _send_float(&f) ;
	f = b ; _send_float(&f) ;
	_get_int(&i) ;
	return i;
}
