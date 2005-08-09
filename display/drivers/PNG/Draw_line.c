
/*
 * draw a line between two given points in the current color.
 *
 * Called by:
 *     Cont_abs() in ../lib/Cont_abs.c
 */

#include <math.h>

#include "pngdriver.h"

static void
store_xy(int x, int y, int pixel_size)
{
	int xi, x_end, yi, y_end;
	double theta;

	if (x < 0 || x >= width || y < 0 || y >= height)
		return;

	grid[y * width + x] = currentColor;

	if(pixel_size == 1)
		return;

	y_end = y + pixel_size / 2;
	for(yi = y - pixel_size / 2; yi <= y_end; yi++)
		store_xy(x, yi, 1);

	x_end = x + pixel_size;
	for(xi = 0; xi <= pixel_size; xi++){
		theta = acos(((double)xi) / (((double)pixel_size) / 2.0));
		yi = (int)(((double)xi) * tan(theta));
		y_end = y + yi;
		for(yi = y - yi; yi <= y_end; yi++){
			store_xy(x - xi, yi, 1);
			store_xy(x + xi, yi, 1);
		}
	}
}

int 
draw_line(int x1, int y1, int x2, int y2)
{
	int x, y, x_end, y_end;
	int xinc, yinc, error;
	int delta_x, delta_y;

	x = x1;
	x_end = x2;
	y = y1;
	y_end = y2;

	if (x == x_end && y == y_end )
	{
		store_xy(x, y, linewidth);
		return 0;
	}

	/* generate equation */
	delta_y = y_end - y;
	delta_x = x_end - x;

	/* figure out which way to move x */
	xinc = 1;
	if (delta_x < 0)
	{
		delta_x = -delta_x;
		xinc = -1;
	}

	/* figure out which way to move y */
	yinc = 1;
	if (delta_y < 0)
	{
		delta_y = -delta_y;
		yinc = -1;
	}

	if (delta_x > delta_y)
	{
		/* always move x, decide when to move y */
		/* initialize the error term, and double delta x and delta y */
		delta_y = delta_y * 2;
		error = delta_y - delta_x;
		delta_x = delta_y - (delta_x * 2);

		while (x != x_end)
		{
		
			store_xy(x, y, linewidth);

			if(error > 0)
			{
				y += yinc;
				error += delta_x;
			}
			else
				error += delta_y;

			x += xinc;
		}
	}
	else
	{
		/* always move y, decide when to move x */
		/* initialize the error term, and double delta x and delta y */
		delta_x = delta_x * 2;
		error = delta_x - delta_y;
		delta_y = delta_x - (delta_y * 2);

		while (y != y_end)
		{
		
			store_xy(x, y, linewidth);

			if(error > 0)
			{
				x += xinc;
				error += delta_y;
			}
			else
				error += delta_x;

			y += yinc;
		}
	}

	store_xy(x, y, linewidth);

	modified = 1;

	return 0;
}

