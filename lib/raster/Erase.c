
#include "raster.h"
#include "graph.h"

int R_erase()
{
	_send_ident(ERASE) ;

	return 0;
}
