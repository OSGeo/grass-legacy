#include "raster.h"
#include "graph.h"

int R_get_location_with_pointer(int *wx,int *wy,int *button )
{
	int z ;
	_send_ident(GET_LOCATION_WITH_POINTER) ;
	z = *wx ;
	_send_int(&z) ;
	z = *wy ;
	_send_int(&z) ;
	z = *button ;
	_send_int(&z) ;
	_get_int(wx) ;
	_get_int(wy) ;
	_get_int(button) ;

	return 0;
}
