#include "driver.h"
#include "driverlib.h"

int Text_size (int x, int y)
{
	text_size_x = (double)x / 25.0 ;
	text_size_y = (double)y / 25.0 ;

	return 0;
}

int Text_rotation (double val)
{
	text_rotation = (double)val ;

	return 0;
}
