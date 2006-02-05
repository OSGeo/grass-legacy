     
#include <stdio.h>
#include <stdlib.h>

#include "gis.h"
#include "colors.h"
#include "pngdriver.h"

static int table_type = FIXED;
static int Red[256], Grn[256], Blu[256];

static void set_color(int i, int red, int grn, int blu)
{
	palette[i][0] = red;
	palette[i][1] = grn;
	palette[i][2] = blu;
	palette[i][3] = 0;
}

void PNG_reset_color(int number, int red, int grn, int blu)
{
	if (table_type != FLOAT)
	{
		G_warning("reset_color: called in FIXED color mode\n");
		return;
	}

	if (number >= NCOLORS || number < 0)
	{
		G_warning("reset_color: can't set color %d\n", number);
		return;
	}

	set_color(number, red, grn, blu);
}

static void init_colors_rgb(void)
{
	NCOLORS = 1<<24;
}

static void init_colors_indexed(void)
{
	int n_pixels;
	int r, g, b;
	int i;

	NCOLORS = 256;

	xpixels = G_realloc(xpixels, NCOLORS * sizeof(unsigned int));
	n_pixels = 0;

	if (has_alpha)
		/* transparent color should be the first!
		 * Its RGB value doesn't matter since we fake RGB-to-index. */
		set_color(n_pixels++, 0, 0, 0);

	for (r = 0; r < 6; r++)
	{
		for (g = 0; g < 6; g++)
		{
			for (b = 0; b < 6; b++)
			{
				int red = r * 0xFF / 5;
				int grn = g * 0xFF / 5;
				int blu = b * 0xFF / 5;

				set_color(n_pixels++, red, grn, blu);
			}
		}
	}

	while (n_pixels < NCOLORS)
		set_color(n_pixels++, 0, 0, 0);

	for (i = 0; i < 256; i++)
		xpixels[i] = i;

	for (i = 0; i < 256; i++)
	{
		int k = i * 6 / 256;

		Red[i] = k * 6 * 6;
		Grn[i] = k * 6;
		Blu[i] = k + has_alpha; /* + 1 for transparent color */
	}
}

void init_color_table(void)
{
	if (true_color)
		init_colors_rgb();
	else
		init_colors_indexed();

	/* Generate lookup for "standard" colors */
	LIB_assign_standard_color(RED,     DRV_lookup_color(255,   0,   0));
	LIB_assign_standard_color(ORANGE,  DRV_lookup_color(255, 128,   0));
	LIB_assign_standard_color(YELLOW,  DRV_lookup_color(255, 255,   0));
	LIB_assign_standard_color(GREEN,   DRV_lookup_color(  0, 255,   0));
	LIB_assign_standard_color(BLUE,    DRV_lookup_color(  0,   0, 255));
	LIB_assign_standard_color(INDIGO,  DRV_lookup_color(  0, 128, 255));
	LIB_assign_standard_color(VIOLET,  DRV_lookup_color(255,   0, 255));
	LIB_assign_standard_color(BLACK,   DRV_lookup_color(  0,   0,   0));
	LIB_assign_standard_color(WHITE,   DRV_lookup_color(255, 255, 255));
	LIB_assign_standard_color(GRAY,    DRV_lookup_color(175, 175, 175));
	LIB_assign_standard_color(BROWN,   DRV_lookup_color(180,  77,  25));
	LIB_assign_standard_color(MAGENTA, DRV_lookup_color(255,   0, 128));
	LIB_assign_standard_color(AQUA,    DRV_lookup_color(100, 128, 255));
}

int PNG_Color_table_float(void)
{
	if (!COM_Can_do_float())
	{
		G_warning("Color_table_float: not available on this device\n");
		return -1;
	}

	table_type = FLOAT;

	COM_Color_offset(0);

	DRV_reset_color(RED, 255, 0, 0);
	DRV_reset_color(ORANGE, 255, 127, 0);
	DRV_reset_color(YELLOW, 255, 255, 0);
	DRV_reset_color(GREEN, 0, 255, 0);
	DRV_reset_color(BLUE, 0, 0, 255);
	DRV_reset_color(INDIGO, 0, 127, 255);
	DRV_reset_color(VIOLET, 255, 0, 255);
	DRV_reset_color(WHITE, 255, 255, 255);
	DRV_reset_color(BLACK, 0, 0, 0);
	DRV_reset_color(GRAY, 127, 127, 127);
	DRV_reset_color(BROWN, 180, 75, 25);
	DRV_reset_color(MAGENTA, 255, 0, 127);
	DRV_reset_color(AQUA, 100, 127, 255);

	return 0;
}

int PNG_Color_table_fixed(void)
{
	table_type = FIXED;

	LIB_assign_fixed_color(RED,     DRV_lookup_color(255,   0,   0));
	LIB_assign_fixed_color(ORANGE,  DRV_lookup_color(255, 128,   0));
	LIB_assign_fixed_color(YELLOW,  DRV_lookup_color(255, 255,   0));
	LIB_assign_fixed_color(GREEN,   DRV_lookup_color(  0, 255,   0));
	LIB_assign_fixed_color(BLUE,    DRV_lookup_color(  0,   0, 255));
	LIB_assign_fixed_color(INDIGO,  DRV_lookup_color(  0, 128, 255));
	LIB_assign_fixed_color(VIOLET,  DRV_lookup_color(255,   0, 255));
	LIB_assign_fixed_color(BLACK,   DRV_lookup_color(  0,   0,   0));
	LIB_assign_fixed_color(WHITE,   DRV_lookup_color(255, 255, 255));
	LIB_assign_fixed_color(GRAY,    DRV_lookup_color(175, 175, 175));
	LIB_assign_fixed_color(BROWN,   DRV_lookup_color(180,  77,  25));
	LIB_assign_fixed_color(MAGENTA, DRV_lookup_color(255,   0, 128));
	LIB_assign_fixed_color(AQUA,    DRV_lookup_color(100, 128, 255));

	return 0;
}

static int get_color_rgb(int r, int g, int b)
{
	return (r << 16) + (g << 8) + b;
}

static int get_color_indexed(int r, int g, int b)
{
	if (has_alpha && transparent == ((r << 16) | (g << 8) | b))
		return 0;

	return Red[r] + Grn[g] + Blu[b];
}

int PNG_lookup_color(int r, int g, int b)
{
	return true_color
		? get_color_rgb(r, g, b)
		: get_color_indexed(r, g, b);
}

int PNG_get_table_type(void)
{
	return table_type;
}

