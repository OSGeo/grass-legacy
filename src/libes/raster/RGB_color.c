#include "raster.h"
#include "graph.h"

int R_RGB_color(unsigned char red,unsigned char grn,unsigned char blu )
{
	unsigned char f ;

	_send_ident(RGB_COLOR) ;
	f = red ; _send_char(&f) ;
	f = grn ; _send_char(&f) ;
	f = blu ; _send_char(&f) ;

	return 0;
}
