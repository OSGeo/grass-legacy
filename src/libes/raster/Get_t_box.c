
#include "graph.h"

R_get_text_box(sometext, t, b, l, r)
	char *sometext ;
	int *t, *b, *l, *r ;
{
	_send_ident(GET_TEXT_BOX) ;
	_send_text(sometext) ;
	_get_int(t) ;
	_get_int(b) ;
	_get_int(l) ;
	_get_int(r) ;
}
