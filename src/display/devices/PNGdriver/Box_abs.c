
#include "pngdriver.h"

int 
Box_abs(int x1, int y1, int x2, int y2)
{
	int tmp;
	int x, y;

	if (x1 > x2)
		tmp = x1, x1 = x2, x2 = tmp;

	if (y1 > y2)
		tmp = y1, y1 = y2, y2 = tmp;

	if (x2 < 0 || x1 > width - 1)
		return;

	if (y2 < 0 || y1 > height - 1)
		return;

	if (x1 < 0)
		x1 = 0;

	if (x2 > width - 1)
		x2 = width - 1;

	if (y1 < 0)
		y1 = 0;

	if (y2 > height - 1)
		y2 = width - 1;

	for (y = y1; y < y2; y++)
	{
		unsigned int *p = &grid[y * width + x1];

		for (x = x1; x < x2; x++)
			*p++ = currentColor;
	}

	return 0;
}

