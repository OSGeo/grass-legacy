#include "raster.h"
#include "graph.h"

int R_get_color_index(float r,float g,float b)
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
