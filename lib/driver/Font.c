#include <stdio.h>
#include <string.h>

#include <grass/gis.h>
#include "driver.h"
#include "driverlib.h"

#define NORMAL      0
#define FREETYPE    1

static int font_type = NORMAL;

void COM_Font_stroke_get(const char *filename)
{
	if (font_init(filename) == 0)
		font_type = NORMAL;
}

void COM_Font_freetype_get(const char *filename)
{
	if (font_init_freetype(filename) == 0)
		font_type = FREETYPE;
}

void COM_Font_get(const char *name)
{
	if (name[0] == '/')
	{
		char prefix[4096];
		FILE *fp;

		fp = fopen(name, "r");
		if (!fp)
			return;
		fclose(fp);

		sprintf(prefix, "%s/fonts/", G_gisbase());

		if (strncmp(name, prefix, strlen(prefix)) == 0)
			COM_Font_stroke_get(name);
		else
			COM_Font_freetype_get(name);
	}
	else
	{
		char filename[4096];
		int i;

		/* check if freetype font is available in freetypecap */
		for (i = 0; ftcap[i].name; i++)
			if (strcmp(name, ftcap[i].name) == 0)
			{
				COM_Font_freetype_get(ftcap[i].path);
				return;
			}

		sprintf(filename, "%s/fonts/%s", G_gisbase(), name);
		COM_Font_stroke_get(filename);
	}
}

void COM_Font_init_charset(const char *charset)
{
	font_init_charset(charset);
}

int font_is_freetype(void)
{
	return font_type == FREETYPE;
}

