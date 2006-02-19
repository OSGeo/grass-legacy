#include <stdlib.h>
#include <grass/colors.h>
#include "includes.h"
#include "XDRIVER.h"

/* draw a line between two given points in the current color. X version */
static XImage *bgimage;
static int x0, y0, cr, cg, cb;

static void store_xy(int x, int y)
{
	int r, g, b;

	if (x < screen_left || x >= screen_right ||
	    y < screen_top  || y >= screen_bottom)
		return;

	DRV_lookup_rgb(XGetPixel(bgimage, x-x0, y-y0), &r, &g, &b);
	XSetForeground(dpy, gc, DRV_lookup_color(
			r*transparency+cr*(1-transparency),
			g*transparency+cg*(1-transparency),
			b*transparency+cb*(1-transparency)));
	XDrawLine(dpy, bkupmap, gc, x, y, x, y);
}

void XD_draw_line(int x1, int y1, int x2, int y2)
{
	int x, y, x_end, y_end;
	int xinc, yinc, error;
	int delta_x, delta_y, width, height;

	if (transparency == 1.0)
		return;

	if (transparency == 0.0)
	{
		XDrawLine(dpy, bkupmap, gc, x1, y1, x2, y2);
		needs_flush = 1;
		return;
	}

	x = x1;
	x_end = x2;
	y = y1;
	y_end = y2;

	x0 = x1;
	y0 = y1;

	DRV_lookup_rgb(current_color, &cr, &cg, &cb);

	if (x == x_end && y == y_end )
	{
		if (x < screen_left || x >= screen_right ||
		    y < screen_top  || y >= screen_bottom)
			return;
		bgimage = XGetImage(dpy, bkupmap, x, y, 1, 1, ~0, ZPixmap);
		store_xy(x, y);
		XDestroyImage(bgimage);
		return;
	}

	/* generate equation */
	delta_y = y_end - y;
	delta_x = x_end - x;

	/* figure out which way to move x */
	xinc = 1;
	if (delta_x < 0)
	{
		x0 = x2;
		delta_x = -delta_x;
		xinc = -1;
	}

	/* figure out which way to move y */
	yinc = 1;
	if (delta_y < 0)
	{
		y0 = y2;
		delta_y = -delta_y;
		yinc = -1;
	}

	if (x0 >= screen_right || y0 >= screen_bottom ||
	    x0+delta_x < screen_left || y0+delta_y < screen_top)
		return;

	if (x0 < screen_left)
		x0 = screen_left;
	if (y0 < screen_top)
		y0 = screen_top;
	width = delta_x + 1;
	height = delta_y + 1;
	if (x0+delta_x >= screen_right)
		width = screen_right - x0;
	if (y0+delta_y >= screen_bottom)
		height = screen_bottom - y0;

	bgimage = XGetImage(dpy, bkupmap, x0, y0, width, height, ~0, ZPixmap);

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

	XDestroyImage(bgimage);

	XSetForeground(dpy, gc, current_color);

	needs_flush = 1;
}

