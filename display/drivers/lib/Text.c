#include "config.h"
#include "driver.h"
#include "driverlib.h"

int Text (char *text)
{

#ifdef HAVE_FT2BUILD_H
	if(isFont_freetype()==-1){
#endif
		soft_text(cur_x, cur_y,text_size_x, text_size_y, text_rotation, text);
#ifdef HAVE_FT2BUILD_H
	}else{
		soft_text_freetype(cur_x, cur_y,text_size_x, text_size_y, text_rotation, text);
	}
#endif

	return 0;
}
