#include <stdio.h>
#include <stdlib.h>
#include <grass/gis.h>
#include "driver.h"
#include "driverlib.h"

/******************************************************************************
 * These routines support the drawing of multi-band images on the graphics
 * device.  A color lookup table with equal divisions in the red, green, and
 * blue dimensions is created for this purpose.
 *
 * The user sends a red, green, and blue intensity ramp for 256 levels each
 * using the Set_RGB_color() routine.  Subsequent calls to RGB_raster uses
 * this information to convert RGB intensity rasters to a color raster.
 * This is then sent to the routine Raster_int().
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
 * RGB_raster(n, nrows, red, grn, blu, withzeros)
 *     int n ;
 *     int nrows ;
 *     unsigned char *red, *grn, *blu ;
 *     int withzeros ;
 * Generates a color Raster_int() call based on the red, grn, and blu array
 * information and the intensity function provided in the last Set_RGB_color()
 * call.
 ******************************************************************************
 */

static unsigned char red[256], grn[256], blu[256];

void COM_RGB_set_colors(
	const unsigned char *r, const unsigned char *g, const unsigned char *b)
{
	if (driver->RGB_set_colors)
		(*driver->RGB_set_colors)(r, g, b);
}

void COM_RGB_raster(
	int n, int nrows,
	const unsigned char *r, const unsigned char *g, const unsigned char *b,
	const unsigned char *nul)
{
	if (driver->RGB_raster)
		(*driver->RGB_raster)(n, nrows, r, g, b, nul);
}

