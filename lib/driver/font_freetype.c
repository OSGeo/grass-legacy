#include <string.h>
#include "driverlib.h"

static char filename[256];
static char charset[50] = "EUC-JP";

int font_init_freetype(const char *name)
{
	strncpy(filename, name, sizeof(filename));
	filename[sizeof(filename) - 1] = '\0';
	return 0;
}

int font_init_charset(const char *str)
{
	strncpy(charset, str, sizeof(charset));
	charset[sizeof(charset) - 1] = '\0';
	return 0;
}

const char *font_get_freetype_name(void)
{
	return filename;
}

const char *font_get_charset(void)
{
	return charset;
}

