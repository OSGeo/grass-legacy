
#include "graph.h"

R_text_size(width, height)
	int width, height ;
{
	int z ;
	_send_ident(TEXT_SIZE) ;
	z = width ;
	_send_int(&z) ;
	z = height ;
	_send_int(&z) ;
}
