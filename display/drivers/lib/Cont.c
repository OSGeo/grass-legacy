#include "driver.h"
#include "driverlib.h"

void COM_Cont_abs(int x, int y)
{
	if (driver->draw_line)
		(*driver->draw_line)(cur_x, cur_y, x, y);
	cur_x = x;
	cur_y = y;
}

void COM_Cont_rel(int x, int y)
{
	COM_Cont_abs(cur_x + x, cur_y + y);
}

