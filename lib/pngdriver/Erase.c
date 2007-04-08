#include "pngdriver.h"

void PNG_Erase(void)
{
	unsigned int color;
	int n, i;

	n = width * height;

	color = (has_alpha && true_color)
		? 0xff000000 :
		PNG_lookup_color(
			(background >> 16) & 0xff,
			(background >>  8) & 0xff,
			(background >>  0) & 0xff);

	for (i = 0; i < n; i++)
		grid[i] = color;

	modified = 1;
}

