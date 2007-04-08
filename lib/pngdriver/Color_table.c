
#include <stdio.h>
#include <stdlib.h>

#include <grass/gis.h>
#include <grass/colors.h>
#include "pngdriver.h"

static int Red[256], Grn[256], Blu[256];

static void set_color(int i, int red, int grn, int blu)
{
	palette[i][0] = red;
	palette[i][1] = grn;
	palette[i][2] = blu;
	palette[i][3] = 0;
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
	{
		int k = i * 6 / 256;

		Red[i] = k * 6 * 6;
		Grn[i] = k * 6;
		Blu[i] = k;
	}
}

void init_color_table(void)
{
	if (true_color)
		init_colors_rgb();
	else
		init_colors_indexed();
}

static int get_color_rgb(int r, int g, int b)
{
	return (r << 16) + (g << 8) + b;
}

static int get_color_indexed(int r, int g, int b)
{
	if (has_alpha && transparent == ((r << 16) | (g << 8) | b))
		return 0;

	return Red[r] + Grn[g] + Blu[b] + has_alpha;
}

int PNG_lookup_color(int r, int g, int b)
{
	return true_color
		? get_color_rgb(r, g, b)
		: get_color_indexed(r, g, b);
}

