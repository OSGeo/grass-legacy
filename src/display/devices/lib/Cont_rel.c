
#include "driver.h"
#include "driverlib.h"

int 
Cont_rel (int x, int y)
{
	Cont_abs(cur_x+x, cur_y+y) ;

	return 0;
}
