
#include "graph.h"

R_text_rotation(rotation)
	float rotation ;
{
	float z ;
	_send_ident(TEXT_ROTATION) ;
	z = rotation ;
	_send_float(&z) ;
}
