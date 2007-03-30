
#include <stdio.h>
#include <stdlib.h>

#include <grass/gis.h>
#include <grass/colors.h>
#include "psdriver.h"

void init_color_table(void)
{
	int n_std_colors = G_num_standard_colors();
	int colorindex;

	/* Generate lookup for "standard" colors */
	for (colorindex = 1; colorindex < n_std_colors; colorindex++)
	{
		struct color_rgb rgb = G_standard_color_rgb(colorindex);

		LIB_assign_standard_color(
			colorindex,
			DRV_lookup_color(rgb.r, rgb.g, rgb.b));
	}
}

static int get_color_rgb(int r, int g, int b)
{
	return (r << 16) + (g << 8) + b;
}

static int get_color_gray(int r, int g, int b)
{
	return (int) (r * 0.299 + g * 0.587 + b * 0.114);
}

int PS_lookup_color(int r, int g, int b)
{
	return true_color
		? get_color_rgb(r, g, b)
		: get_color_gray(r, g, b);
}

