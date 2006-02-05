#include "driver.h"
#include "driverlib.h"

#define NORMAL      0
#define FREETYPE    1

#define RELEASE     0
#define HOLD        1

static int font_type = NORMAL;
static int font_hold = RELEASE;

int COM_Font_get(const char *filename)
{
	if (font_hold==HOLD)
		return 0;
	font_type = NORMAL;

	return init_font(filename);
}

int COM_Font_freetype_get(const char *filename)
{
	font_hold = HOLD;
	if (init_font_freetype(filename) == 0)
	{
		font_type = FREETYPE;
		return 0;
	}
	return -1;
}

int COM_Font_freetype_release(void)
{
	font_hold = RELEASE;
	return 0;
}

int COM_Font_init_charset(const char *charset)
{
	init_font_charset(charset);
	return 0;
}

int font_is_freetype(void)
{
	return font_type == FREETYPE ? 0 : -1;
}

