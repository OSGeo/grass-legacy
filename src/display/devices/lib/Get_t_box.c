#include "driver.h"
#include "driverlib.h"
int 
Get_text_box (char *text, int *t, int *b, int *l, int *r)
{
	soft_text_ext(cur_x, cur_y, 
		text_size_x, text_size_y, text_rotation, text) ;
	get_text_ext (t, b, l, r) ;

	return 0;
}
