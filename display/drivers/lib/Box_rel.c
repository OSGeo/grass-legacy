#include "driver.h"
#include "driverlib.h"

int 
Box_rel (int x, int y)
{
	Box_abs(cur_x, cur_y, cur_x + x, cur_y + y) ;

	return 0;
}
