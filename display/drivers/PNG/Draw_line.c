/*
 * draw a line between two given points in the current color.
 *
 * Called by:
 *     Cont_abs() in ../lib/Cont_abs.c
 */

#include <math.h>
#include "pngdriver.h"

static int cr, cg, cb;

static void store_xy(int x, int y)
{
	int xi, xi_end, yi, yi_end, ind;
	double theta;
	int c, r, g, b;

	if (x < 0 || x >= width || y < 0 || y >= height)
		return;

	ind = y * width + x;
	c = currentColor;
	if (transparency > 0.0) {
		DRV_lookup_rgb(grid[ind], &r, &g, &b);
		c = DRV_lookup_color(
				r*transparency+cr*(1-transparency),
				g*transparency+cg*(1-transparency),
				b*transparency+cb*(1-transparency));
	}
	grid[ind] = c;

	if(linewidth <= 1)
		return;

	xi = linewidth / 2;
	xi_end = x + xi;
	for(xi = x - xi; xi < xi_end; xi++){
		theta = acos(((double)(xi - x)) / (((double)linewidth) / 2.0));
		yi = (int)(((double)(xi - x)) * tan(theta));
		if(xi == x)
			yi = linewidth / 2;
		yi_end = y + yi;
		for(yi = y - yi; yi < yi_end; yi++) {
			if(xi >= 0 && xi < width && yi >= 0 && yi < height) {
				ind = yi * width + xi;
				c = currentColor;
				if (transparency > 0.0) {
					DRV_lookup_rgb(grid[ind], &r, &g, &b);
					c = DRV_lookup_color(
					    r*transparency+cr*(1-transparency),
					    g*transparency+cg*(1-transparency),
					    b*transparency+cb*(1-transparency));
				}
				grid[ind] = c;
			}
		}
	}
}

void PNG_draw_line(int x1, int y1, int x2, int y2)
{
	int x, y, x_end, y_end;
	int xinc, yinc, error;
	int delta_x, delta_y;

	if (transparency == 1.0)
		return;

	x = x1;
	x_end = x2;
	y = y1;
	y_end = y2;

	if (transparency > 0.0)
		DRV_lookup_rgb(currentColor, &cr, &cg, &cb);

	if (x == x_end && y == y_end )
	{
		store_xy(x, y);
		return;
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
		
			store_xy(x, y);

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
		
			store_xy(x, y);

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

	store_xy(x, y);

	modified = 1;
}

