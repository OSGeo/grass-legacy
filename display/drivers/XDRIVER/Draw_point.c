#include "includes.h"
/* draw a point in the current color. X version */

void XD_draw_point(int x, int y)
{
	XImage *bgimage;
	int r, g, b, r2, g2, b2;

	if (x < screen_left || x >= screen_right  ||
	    y < screen_top  || y >= screen_bottom ||
	    transparency == 1.0)
		return;

	if (transparency == 0.0)
	{
		XDrawPoint(dpy, bkupmap, gc, x, y);
		needs_flush = 1;
		return;
	}
	bgimage = XGetImage(dpy, bkupmap, x, y, 1, 1, ~0, ZPixmap);
	DRV_lookup_rgb(XGetPixel(bgimage, 0, 0), &r, &g, &b);
	DRV_lookup_rgb(current_color, &r2, &g2, &b2);
	XSetForeground(dpy, gc, DRV_lookup_color(
				r*transparency+r2*(1-transparency),
				g*transparency+g2*(1-transparency),
				b*transparency+b2*(1-transparency)));
	XDrawPoint(dpy, bkupmap, gc, x, y);
	XDestroyImage(bgimage);
	XSetForeground(dpy, gc, current_color);
	needs_flush = 1;
}

