#include <stdio.h>
#include "includes.h"
#include "../lib/colors.h"
#include "../lib/driver.h"

extern Display *dpy;
extern Window grwin;
extern GC gc;
extern Pixmap bkupmap;
extern int backing_store;
extern u_long *xpixels;
extern Colormap fixedcmap;

char *malloc(), *realloc();

XImage *grimage;
static short alloc = 0, gotimage = 0;
typedef unsigned char byte;
XColor defs;
int tmp = 0;


/* Write 'nrows' lines of 'num' pixels contained as ints in 'array' to
 * the screen starting at the current position. 'withzeros' indicates
 * that * zero-valued pixels in array should either be written or left
 * unchanged. 'color_type' indicates that the pixel values should
 * either be written directly to the screen or used to reference a
 * color look-up table. */

Raster_int(num, nrows, array, withzeros, color_type)
int num, nrows;
unsigned int *array;
int withzeros, color_type;
{
    int i, j;
    unsigned int *arr ;
    byte cur_color ;
    char *calloc(), *realloc();
    XWindowAttributes xwa;
    byte *imdata;
    int _get_color_index();
    int   do_nothing();
    Pixmap pixmap;
    int bytes_per_pixel;
    int offset = 0 ;
    if (! offset)
        offset = get_color_offset() + get_max_std_colors() ;


    /* Unless this is 1st time thru or if raster line length has
         * changed we don't need to reallocate space or re-create the
         * Ximage. */
    if (alloc < num) {
	int pad;
        if (gotimage) {
            XDestroyImage(grimage);     /* destroy any previous images */
            gotimage = 0;
        }
        if (XGetWindowAttributes(dpy, grwin, &xwa) == 0)
            return (-1);
	if (xwa.depth > 8 ) pad = 32;
	else pad = 8;
        grimage = XCreateImage(dpy, xwa.visual, xwa.depth, ZPixmap,
            0, None, num, 1, pad, 0);
        gotimage = 1;
#ifdef DEBUG
{
static int first = 1;
if (first) {
printf("visual = %d, depth = %d\n",xwa.visual->class,xwa.depth);
printf("ximage->width = %d\n",grimage->width);
printf("ximage->height = %d\n",grimage->height);
printf("ximage->xoffset = %d\n",grimage->xoffset);
printf("ximage->format = %d\n",grimage->format);
printf("ximage->byte_order %d\n",grimage->byte_order);
printf("ximage->bitmap_unit = %d\n",grimage->bitmap_unit);
printf("ximage->bitmap_bit_order = %d\n",grimage->bitmap_bit_order);
printf("ximage->bitmap_pad = %d\n",grimage->bitmap_pad);
printf("ximage->depth = %d\n",grimage->depth);
printf("ximage->bytes_per_line = %d\n",grimage->bytes_per_line);
printf("ximage->bits_per_pixel = %d\n",grimage->bits_per_pixel);
printf("ximage->red_mask = %d\n",grimage->red_mask);
printf("ximage->green_mask = %d\n",grimage->green_mask);
printf("ximage->blue_mask = %d\n",grimage->blue_mask);
first = 0;
}}
#endif
        bytes_per_pixel = (grimage->bits_per_pixel + 7)/8;
        if (alloc == 0)
            grimage->data =  malloc(num * bytes_per_pixel);
        else
            grimage->data =  realloc(grimage->data, num * bytes_per_pixel);
        if (grimage->data == NULL)
            return (-1);        /* not enough space left */
    }
/* If zeros are to be included, an entire raster row can be constructed */
    if (withzeros) {
        char *pix;

        if (color_type)
            _get_color_index_array(array, num) ;

        arr = array;
        pix = grimage->data;
        if (get_table_type() == FLOAT) {
	    for (i = 0; i < num; i++)
		XPutPixel(grimage, i, 0, (u_long) *arr++);
        }
        else {
	    for (i = 0; i < num; i++)
		XPutPixel(grimage, i, 0, (u_long) xpixels[*arr++]);
        }

        for (i = 0; i < nrows; i++) {
            XPutImage(dpy, grwin, gc, grimage, 0, 0, cur_x, cur_y + i, num, 1);
            if (!backing_store) {
                XPutImage(dpy, bkupmap, gc, grimage, 0, 0, cur_x, cur_y + i,
                    num, 1);
            }
        }
    }
    /* If zeros are not included may need to draw many shorter rasters.
         * If the pixel value in array is zero we don't disturb the
         * existing pixel of the drawable. If the pixel is non-zero we
         * re-write it. */
    else {
        int start_col, width;

        arr = array;
        start_col = 0;
        width = 0;
        for (j = 0; j < num; j++) {
            if (*arr == 0) {
                if (width > 0) {
                    for (i = 0; i < nrows; i++) {
                        XPutImage(dpy, grwin, gc, grimage, 0, 0,
                            cur_x + start_col, cur_y + i, width, 1);
                        if (!backing_store)
                            XPutImage(dpy, bkupmap, gc, grimage, 0, 0,
                                cur_x + start_col, cur_y + i, width, 1);
                    }
                    width = 0;
                    start_col = j + 1;
                } else {
                    start_col++;
                }
            } else {            /* non-zero pixel, put into the image */
                if (get_table_type() == FLOAT) {
                    if ( color_type )
                        XPutPixel(grimage, width++, 0,
                            (u_long) _get_color_index(*arr));
                    else 
                        XPutPixel(grimage, width++, 0,
                            (u_long) *arr);
                } else {
                    if ( color_type )
                        XPutPixel(grimage, width++, 0,
                            (u_long) xpixels[_get_color_index(*arr)]);
                        else
                        XPutPixel(grimage, width++, 0,
                            (u_long) xpixels[_get_color_index(*arr)]);
                }
            }
            arr++;
        }
        /* Flush out any remaining data */
        if (width > 0) {
            for (i = 0; i < nrows; i++) {
                XPutImage(dpy, grwin, gc, grimage, 0, 0,
                    cur_x + start_col, cur_y + i, width, 1);
                if (!backing_store)
                    XPutImage(dpy, bkupmap, gc, grimage, 0, 0,
                        cur_x + start_col, cur_y + i, width, 1);
            }
        }
    }
    return 1;
}

static do_nothing(n)
{
    return (n);
}
