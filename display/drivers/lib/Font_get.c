#include "driverlib.h"

#define NORMAL      0
#define FREETYPE    1
#define RELEASE     0
#define HOLD        1

static int font_type = NORMAL;
static int font_hold = RELEASE;

/* Font() conflicts with an X declaration */
int Font_get (char *filename)
{
    if(font_hold==HOLD)
	    return 0;
    font_type = NORMAL;
    return (init_font(filename));
}

int Font_freetype_get (char* filename) {
	font_hold = HOLD;
	if(init_font_freetype(filename)==0) {
		font_type = FREETYPE;
		return 0;
	}
    return -1;
}

int Font_freetype_release() {
	font_hold = RELEASE;
	return 0;
}

int isFont_freetype() {
	if(font_type==FREETYPE) {
		return 0;
	}
	return -1;
}
