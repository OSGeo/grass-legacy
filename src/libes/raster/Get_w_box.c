#include "raster.h"
#include "graph.h"

int R_get_location_with_box(int cx,int cy,int *wx,int *wy,int *button)
{
	int z ;
	_send_ident(GET_LOCATION_WITH_BOX) ;
	z = cx ;
	_send_int(&z) ;
	z = cy ;
	_send_int(&z) ;
	z = *wx ;
	_send_int(&z) ;
	z = *wy ;
	_send_int(&z) ;
	_get_int(wx) ;
	_get_int(wy) ;
	_get_int(button) ;

	return 0;
}
