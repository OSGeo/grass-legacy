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

void COM_RGB_raster(
	int n, int nrows,
	const unsigned char *r, const unsigned char *g, const unsigned char *b,
	const unsigned char *nul)
{
	if (driver->RGB_raster)
		(*driver->RGB_raster)(n, nrows, r, g, b, nul);
}

void COM_begin_scaled_raster(int src[2][2], int dst[2][2])
{
	if (driver->Begin_scaled_raster)
		(*driver->Begin_scaled_raster)(src, dst);
	else
		LIB_begin_scaled_raster(src, dst);
}

int COM_scaled_raster(
	int n, int row,
	unsigned char *red, unsigned char *grn, unsigned char *blu,
	unsigned char *nul)
{
	if (driver->Scaled_raster)
		return (*driver->Scaled_raster)(n, row, red, grn, blu, nul);
	else
		return LIB_scaled_raster(n, row, red, grn, blu, nul);
}

