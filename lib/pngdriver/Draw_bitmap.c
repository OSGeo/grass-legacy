
/*
 * draw a line between two given points in the current color.
 *
 * Called by:
 *     Cont_abs() in ../lib/Cont_abs.c
 */

#include <math.h>

#include "pngdriver.h"

#ifndef min
#define min(a,b) ((a)<(b)?(a):(b))
#endif
#ifndef max
#define max(a,b) ((a)>(b)?(a):(b))
#endif

void PNG_draw_bitmap(int ncols, int nrows, int threshold, const unsigned char *buf)
{
	int i0 = max(clip_left - cur_x, 0);
	int i1 = min(clip_rite - cur_x, ncols);
	int j0 = max(clip_top  - cur_y, 0);
	int j1 = min(clip_bot  - cur_y, nrows);

	if (!true_color)
	{
		int i, j;

		for (j = j0; j < j1; j++)
		{
			int y = cur_y + j;

			for (i = i0; i < i1; i++)
			{
				int x = cur_x + i;
				unsigned int k = buf[j * ncols + i];
				unsigned int *p = &grid[y * width + x];

				if (k > threshold)
					*p = currentColor;
			}
		}
	}
	else
	{
		unsigned int a1 = (currentColor >> 24) & 0xFF;
		unsigned int r1 = (currentColor >> 16) & 0xFF;
		unsigned int g1 = (currentColor >>  8) & 0xFF;
		unsigned int b1 = (currentColor >>  0) & 0xFF;
		int i, j;

		for (j = j0; j < j1; j++)
		{
			int y = cur_y + j;

			for (i = i0; i < i1; i++)
			{
				int x = cur_x + i;
				unsigned int k = buf[j * ncols + i];
				unsigned int *p = &grid[y * width + x];
				unsigned int a0 = (*p >> 24) & 0xFF;
				unsigned int r0 = (*p >> 16) & 0xFF;
				unsigned int g0 = (*p >>  8) & 0xFF;
				unsigned int b0 = (*p >>  0) & 0xFF;
				unsigned int a = (a0 * (255 - k) + a1 * k) / 255;
				unsigned int r = (r0 * (255 - k) + r1 * k) / 255;
				unsigned int g = (g0 * (255 - k) + g1 * k) / 255;
				unsigned int b = (b0 * (255 - k) + b1 * k) / 255;

				*p = (a << 24) | (r << 16) | (g << 8) | b;
			}
		}
	}

	modified = 1;
}

