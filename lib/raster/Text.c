#include "raster.h"
#include "graph.h"

int R_text(char *sometext )
{
	_send_ident(TEXT) ;
	_send_text(sometext) ;

	return 0;
}
