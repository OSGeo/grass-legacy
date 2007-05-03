#include <stdio.h>
#include <string.h>

#include <grass/gis.h>
#include "driver.h"
#include "driverlib.h"

#define STROKE      0
#define FREETYPE    1

static int font_type = STROKE;

static void stroke_set(const char *filename)
{
	if (font_init(filename) == 0)
		font_type = STROKE;
}

static void freetype_set(const char *filename)
{
	if (font_init_freetype(filename) == 0)
		font_type = FREETYPE;
}

void COM_Font_get(const char *name)
{
	if (name[0] == '/')
	{
		static char prefix[4096];

		if (!font_exists(name))
			return;

		if (!*prefix)
			sprintf(prefix, "%s/fonts/", G_gisbase());

		if (strncmp(name, prefix, strlen(prefix)) == 0)
			stroke_set(name);
		else
			freetype_set(name);
	}
	else
	{
		char filename[4096];
		int i;

		/* check if freetype font is available in freetypecap */
		for (i = 0; ftcap[i].name; i++)
			if (strcmp(name, ftcap[i].name) == 0)
			{
				freetype_set(ftcap[i].path);
				return;
			}

		sprintf(filename, "%s/fonts/%s", G_gisbase(), name);
		stroke_set(filename);
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

