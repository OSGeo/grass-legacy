#include "driver.h"
#include "driverlib.h"

int Text (char *text)
{
	soft_text(cur_x, cur_y, 
		text_size_x, text_size_y, text_rotation, text) ;

	return 0;
}
