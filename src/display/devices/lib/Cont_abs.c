
#include "driver.h"

Cont_abs(x,y)
	int x, y ;
{
	draw_line(cur_x, cur_y, x, y) ;
	cur_x = x ;
	cur_y = y ;
}
