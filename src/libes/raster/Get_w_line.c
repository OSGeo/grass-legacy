
#include "graph.h"

R_get_location_with_line(cx, cy, wx, wy, button)
	int cx, cy ;
	int *wx, *wy ;
	int *button ;
{
	int z ;
	_send_ident(GET_LOCATION_WITH_LINE) ;
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
}
