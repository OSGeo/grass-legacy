#include "driverlib.h"

int Erase (void)
{
	int top, bot, left, rite ;

	Screen_top(&top) ;
	Screen_bot(&bot) ;
	Screen_rite(&rite) ;
	Screen_left(&left) ;

	Box_abs(left, top, rite, bot) ;

	return 0;
}
