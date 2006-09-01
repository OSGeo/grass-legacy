#include <stdio.h>
#include <string.h>

#include "driver.h"
#include "driverlib.h"

#define NORMAL      0
#define FREETYPE    1

static int font_type = NORMAL;

int COM_Font_stroke_get(const char *name)
{
	font_type = NORMAL;
	return font_init(name);
}

int COM_Font_freetype_get(const char *filename)
{
	if (font_init_freetype(filename) == 0)
	{
		font_type = FREETYPE;
		return 0;
	}
	return -1;
}

int COM_Font_get(const char *name)
{
	if (name[0] == '/')
	{
		char prefix[4096];
		FILE *fp;

		fp = fopen(name, "r");
		if (!fp)
			return -1;
		fclose(fp);

		sprintf(prefix, "%s/fonts/", G_gisbase());

		return (strncmp(name, prefix, strlen(prefix)) == 0)
			? COM_Font_stroke_get(name)
			: COM_Font_freetype_get(name);
	}
	else
	{
		char filename[4096];
		int i;

		/* check if freetype font is available in freetypecap */
		for (i = 0; ftcap[i].name; i++)
			if (strcmp(name, ftcap[i].name) == 0)
				return COM_Font_freetype_get(ftcap[i].path);

		sprintf(filename, "%s/fonts/%s", G_gisbase(), name);
		return COM_Font_stroke_get(filename);
	}

	/* can't happen */
	return -1;
}

int COM_Font_freetype_release(void)
{
	return 0;
}

int COM_Font_init_charset(const char *charset)
{
	font_init_charset(charset);
	return 0;
}

int font_is_freetype(void)
{
	return font_type == FREETYPE;
}

