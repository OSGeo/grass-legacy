#include "graphics.h"

Text_size(x, y)
	int x, y ;
{
	_text_size_x = (float)x / 4.0 ;
	_text_size_y = (float)y / 4.0 ;
}

Text_rotation(val)
	float val ;
{
	_text_rotation = (double)val ;
}
