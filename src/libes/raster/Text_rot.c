#include "raster.h"
#include "graph.h"

int R_text_rotation(float rotation )
{
	float z ;
	_send_ident(TEXT_ROTATION) ;
	z = rotation ;
	_send_float(&z) ;

	return 0;
}
