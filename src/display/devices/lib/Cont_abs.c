
#include "driver.h"
#include "driverlib.h"

int 
Cont_abs (int x, int y)
{
	draw_line(cur_x, cur_y, x, y) ;
	cur_x = x ;
	cur_y = y ;

	return 0;
}
