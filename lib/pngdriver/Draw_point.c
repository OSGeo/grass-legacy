#include "pngdriver.h"

void PNG_draw_point(int x, int y)
{
	if (x < 0 || x >= width || y < 0 || y >= height)
		return;

	grid[y * width + x] = currentColor;

	modified = 1;
}

