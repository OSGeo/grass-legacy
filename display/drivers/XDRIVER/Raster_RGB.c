/******************************************************************************
 * These routines support the drawing of multi-band images on the graphics
 * device.  A color lookup table with equal divisions in the red, green, and
 * blue dimensions is created for this purpose.
 *
 * The user sends a red, green, and blue intensity ramp for 256 levels each
 * using the Set_RGB_color() routine.  Subsequent calls to RGB_raster uses
 * this information to convert RGB intensity rasters to a color raster.
 *
 * All intensity values are represented in unsigned (8-byte) values.  That is
 * with values between and including 0 and 255.
 *
 ******************************************************************************
 * RGB_raster(n, nrows, red, grn, blu, nul)
 *     int n ;
 *     int nrows ;
 *     unsigned char *red, *grn, *blu, *nul ;
 * Renders the data based on the red, grn, and blu array information
 * and the intensity function provided in the last Set_RGB_color()
 * call.
 ******************************************************************************
 */
#include <stdio.h>
#include <stdlib.h>
#include "includes.h"
#include <grass/gis.h>
#include "XDRIVER.h"

extern unsigned long find_color(unsigned int r, unsigned int g, unsigned int b);

void XD_RGB_raster(
	int n, int nrows,
	const unsigned char *r, const unsigned char *g, const unsigned char *b,
	const unsigned char *nul)
{
	XWindowAttributes attr;
	XImage *img;
	int i;

	XGetWindowAttributes(dpy, grwin, &attr);

	img = XCreateImage(dpy, attr.visual, attr.depth, ZPixmap,
			   0, NULL, n, 1, 8, 0);

	img->data = G_malloc(img->bytes_per_line + 4);
	*(int *)(img->data + img->bytes_per_line) = 0xDEADBEEF;

	if (!img)
		G_fatal_error("unable to allocate XImage");

	for (i = 0; i < n; )
	{
		int i0, j;

		for ( ; i < n && nul && nul[i]; i++)
			;

		i0 = i;

		for ( ; i < n && (!nul || !nul[i]); i++)
		{
			unsigned long pixel = find_color(r[i], g[i], b[i]);
			XPutPixel(img, i, 0, pixel);
		}

		if (i == i0)
			continue;

		for (j = 0; j < nrows; j++)
			XPutImage(dpy, bkupmap, gc, img, i0, 0, cur_x + i0, cur_y + j, i - i0, 1);
	}

	XDestroyImage(img);
	needs_flush = 1;
}

