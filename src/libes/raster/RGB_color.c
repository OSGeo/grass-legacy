
#include "graph.h"

R_RGB_color(red, grn, blu)
	unsigned char red, grn, blu ;
{
	unsigned char f ;

	_send_ident(RGB_COLOR) ;
	f = red ; _send_char(&f) ;
	f = grn ; _send_char(&f) ;
	f = blu ; _send_char(&f) ;
}
