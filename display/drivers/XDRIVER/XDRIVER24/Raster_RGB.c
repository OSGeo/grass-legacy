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
 * Set_RGB_color(r,g,b)
 *     unsigned char r[256], g[256], b[256] ;
 * This contains the desired intensity functions for red, green, and blue.
 * Using the known number of available levels static arrays are filled with
 * which provide easy determination of which real color is associated with
 * any given RGB color intensity cmbination.
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
#include "gis.h"
#include "driverlib.h"
#include "driver.h"
#include "XDRIVER.h"

#ifndef min
#define min(a, b) ((a) < (b) ? (a) : (b))
#endif

static int red[256], grn[256], blu[256];

int 
Set_RGB_color(unsigned char r[256], unsigned char g[256], unsigned char b[256])
{
	int i;

	for(i = 0; i < 256; i++)
	{
		red[i] = r[i];
		grn[i] = g[i];
		blu[i] = b[i];
	}

	return 0;
}

int 
RGB_raster(int n, int nrows,
	   unsigned char *r, unsigned char *g, unsigned char *b,
	   unsigned char *nul)
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
			int rr = red[r[i]];
			int gg = grn[g[i]];
			int bb = blu[b[i]];
			unsigned long pixel = find_color(rr, gg, bb);

			XPutPixel(img, i, 0, pixel);
		}

		if (i == i0)
			continue;

		for (j = 0; j < nrows; j++)
			XPutImage(dpy, bkupmap, gc, img, i0, 0, cur_x + i0, cur_y + j, i - i0, 1);
	}

	XDestroyImage(img);
	needs_flush = 1;

	return 0;
}

