#include "raster.h"
#include "graph.h"

int R_color(int index)
{
	int i ;
	i = index ;
	_send_ident(COLOR) ;
	_send_int(&i) ;

	return 0;
}

int R_standard_color(int index)
{
	int i ;
	i = index ;
	_send_ident(STANDARD_COLOR) ;
	_send_int(&i) ;

	return 0;
}
