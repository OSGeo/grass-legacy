
#include "graph.h"

R_text(sometext)
	char *sometext ;
{
	_send_ident(TEXT) ;
	_send_text(sometext) ;
}
