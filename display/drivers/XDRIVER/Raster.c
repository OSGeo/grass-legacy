#include <stdio.h>
#include <stdlib.h>
#include <grass/gis.h>
#include "includes.h"
#include <grass/colors.h>
#include "XDRIVER.h"

static XImage *grimage;
static int alloc;
static int gotimage;

/* Write 'nrows' lines of 'num' pixels contained as ints in 'array' to
 * the screen starting at the current position. 'withzeros' indicates
 * that * zero-valued pixels in array should either be written or left
 * unchanged. 'color_type' indicates that the pixel values should
 * either be written directly to the screen or used to reference a
 * color look-up table. */

void XD_Raster_int(int num, int nrows, const int *array, int withzeros, int color_type)
{
	XWindowAttributes xwa;
	int bytes_per_pixel;
	int i;

	/* Unless this is 1st time thru or if raster line length has
	 * changed we don't need to reallocate space or re-create the
	 * Ximage. */
	if (alloc < num)
	{
		int pad;

		if (gotimage)
		{
			XDestroyImage(grimage);     /* destroy any previous images */
			gotimage = 0;
		}

		if (!XGetWindowAttributes(dpy, grwin, &xwa))
			return;

		pad = (xwa.depth > 8) ? 32 : 8;

		grimage = XCreateImage(dpy, xwa.visual, xwa.depth, ZPixmap,
				       0, None, num, 1, pad, 0);
		gotimage = 1;

#ifdef DEBUG
		{
			static int first = 1;
			if (first)
			{
				fprintf (stdout,"visual = %d, depth = %d\n",xwa.visual->class,xwa.depth);
				fprintf (stdout,"ximage->width = %d\n",grimage->width);
				fprintf (stdout,"ximage->height = %d\n",grimage->height);
				fprintf (stdout,"ximage->xoffset = %d\n",grimage->xoffset);
				fprintf (stdout,"ximage->format = %d\n",grimage->format);
				fprintf (stdout,"ximage->byte_order %d\n",grimage->byte_order);
				fprintf (stdout,"ximage->bitmap_unit = %d\n",grimage->bitmap_unit);
				fprintf (stdout,"ximage->bitmap_bit_order = %d\n",grimage->bitmap_bit_order);
				fprintf (stdout,"ximage->bitmap_pad = %d\n",grimage->bitmap_pad);
				fprintf (stdout,"ximage->depth = %d\n",grimage->depth);
				fprintf (stdout,"ximage->bytes_per_line = %d\n",grimage->bytes_per_line);
				fprintf (stdout,"ximage->bits_per_pixel = %d\n",grimage->bits_per_pixel);
				fprintf (stdout,"ximage->red_mask = %d\n",grimage->red_mask);
				fprintf (stdout,"ximage->green_mask = %d\n",grimage->green_mask);
				fprintf (stdout,"ximage->blue_mask = %d\n",grimage->blue_mask);
				first = 0;
			}
		}
#endif
		bytes_per_pixel = (grimage->bits_per_pixel + 7)/8;

		grimage->data =  G_realloc(grimage->data, num * bytes_per_pixel);
	}

	/* If zeros are to be included, an entire raster row can be constructed */
	if (withzeros)
	{
		if (color_type)
		{
			static int *array2;
			static int nalloc;

			if (num > nalloc)
			{
				nalloc = num;
				array2 = G_realloc(array2, nalloc * sizeof(int));
			}

			LIB_get_color_index_array(array2, array, num);
			array = array2;
		}

		if (use_visual->class >= TrueColor)
			for (i = 0; i < num; i++)
				XPutPixel(grimage, i, 0, (u_long) array[i]);
		else
			for (i = 0; i < num; i++)
				XPutPixel(grimage, i, 0, (u_long) xpixels[array[i]]);

		for (i = 0; i < nrows; i++)
			XPutImage(dpy, bkupmap, gc, grimage, 0, 0, cur_x, cur_y + i, num, 1);
	}
	/* If zeros are not included may need to draw many shorter rasters.
	 * If the pixel value in array is zero we don't disturb the
	 * existing pixel of the drawable. If the pixel is non-zero we
	 * re-write it. */
	else
	{
		int start_col, width;

		start_col = 0;
		width = 0;

		for (i = 0; i < num; i++)
		{
			int c = array[i];

			if (c == 0)
			{
				if (width > 0)
				{
					int j;

					for (j = 0; j < nrows; j++)
						XPutImage(dpy, bkupmap, gc, grimage, 0, 0,
							  cur_x + start_col, cur_y + j, width, 1);

					width = 0;
					start_col = i + 1;
				}
				else
					start_col++;
			}
			else
			{
				if (color_type)
					c = LIB_get_color_index(c);

				/* non-zero pixel, put into the image */
				if (use_visual->class >= TrueColor)
					XPutPixel(grimage, width++, 0, (u_long) c);
				else
					XPutPixel(grimage, width++, 0, (u_long) xpixels[c]);
			}
		}
		/* Flush out any remaining data */
		if (width > 0)
		{
			int j;

			for (j = 0; j < nrows; j++)
				XPutImage(dpy, bkupmap, gc, grimage, 0, 0,
					  cur_x + start_col, cur_y + j, width, 1);
		}
	}

	needs_flush = 1;
}

