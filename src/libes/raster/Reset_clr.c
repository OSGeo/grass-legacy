
#include "graph.h"

R_reset_color(red, grn, blu, number)
	unsigned char red, grn, blu ;
	int number ;
{
	int i ;
	unsigned char f ;

	i = number ;
	if (i < 0)
		i = 256 - i ;

	_send_ident(RESET_COLOR) ;
	f = red ; _send_char(&f) ;
	f = grn ; _send_char(&f) ;
	f = blu ; _send_char(&f) ;
	_send_int(&i) ;
}
