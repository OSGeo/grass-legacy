/* %W% %G% */

#include "driver.h"
Text(text)
	char *text ;
{
	if (*text != '@') soft_text(cur_x, cur_y, 
		_text_size_x, _text_size_y, _text_rotation, text) ;
	else graf_text(cur_x, cur_y+1, text+1);
}
