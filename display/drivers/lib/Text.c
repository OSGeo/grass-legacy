#include "driver.h"
#include "driverlib.h"

int Text (char *text)
{

	if(isFont_freetype()==-1){
		soft_text(cur_x, cur_y,text_size_x, text_size_y, text_rotation, text);
	}else{
		soft_text_freetype(cur_x, cur_y,text_size_x, text_size_y, text_rotation, text);
	}

	return 0;
}
