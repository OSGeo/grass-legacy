#include "raster.h"
#include "graph.h"

int R_text_size( int width,int height )
{
	int z ;
	_send_ident(TEXT_SIZE) ;
	z = width ;
	_send_int(&z) ;
	z = height ;
	_send_int(&z) ;

	return 0;
}
