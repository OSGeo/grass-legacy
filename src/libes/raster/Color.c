#include "graph.h"

R_color(index)
	int index ;
{
	int i ;
	i = index ;
	_send_ident(COLOR) ;
	_send_int(&i) ;
}

R_standard_color(index)
	int index ;
{
	int i ;
	i = index ;
	_send_ident(STANDARD_COLOR) ;
	_send_int(&i) ;
}
