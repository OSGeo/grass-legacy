#include "config.h"
#include "driver.h"
#include "driverlib.h"

int 
Get_text_box (char *text, int *t, int *b, int *l, int *r)
{
#ifdef HAVE_FT2BUILD_H
	if(isFont_freetype()==-1) {
#endif
		soft_text_ext(cur_x, cur_y, 
			text_size_x, text_size_y, text_rotation, text) ;
		get_text_ext (t, b, l, r) ;
#ifdef HAVE_FT2BUILD_H
	}else{
		soft_text_ext_freetype(cur_x, cur_y, 
			text_size_x, text_size_y, text_rotation, text) ;
		get_text_ext_freetype (t, b, l, r) ;
	}
#endif
	return 0;
}
