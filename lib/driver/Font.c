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
	if (G_is_absolute_path(name))
	{
		if (!font_exists(name))
			return;
	   
		freetype_set(name);
	}   
	else
	{
		int i;

		/* check if freetype font is available in freetypecap */
		for (i = 0; ftcap[i].name; i++)
			if (strcmp(name, ftcap[i].name) == 0)
			{
				freetype_set(ftcap[i].path);
				return;
			}

		stroke_set(name);
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

void COM_Font_list(char ***list, int *count)
{
	char **fonts;
	int num_fonts;
	int i;

	for (i = 0; ftcap[i].name; i++)
		;

	num_fonts = i;

	fonts = G_malloc(num_fonts * sizeof(const char *));

	for (i = 0; i < num_fonts; i++)
		fonts[i] = G_store(ftcap[i].name);

	*list = fonts;
	*count = num_fonts;
}

void free_font_list(char **fonts, int count)
{
	int i;

	for (i = 0; i < count; i++)
		G_free(fonts[i]);
	G_free(fonts);
}
