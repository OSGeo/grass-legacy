#include "raster.h"
#include "graph.h"

int R_get_text_box( char *sometext, int *t,int *b,int *l,int *r)
{
	_send_ident(GET_TEXT_BOX) ;
	_send_text(sometext) ;
	_get_int(t) ;
	_get_int(b) ;
	_get_int(l) ;
	_get_int(r) ;

	return 0;
}
