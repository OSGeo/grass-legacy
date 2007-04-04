#include <stdio.h>
#include <stdlib.h>
#include <grass/gis.h>
#include "driver.h"
#include "driverlib.h"

/******************************************************************************
 * These routines support the drawing of multi-band images on the graphics
 * device.
 ******************************************************************************
 */

void COM_begin_scaled_raster(int src[2][2], int dst[2][2])
{
	if (driver->Begin_scaled_raster)
		(*driver->Begin_scaled_raster)(src, dst);
}

int COM_scaled_raster(
	int n, int row,
	unsigned char *red, unsigned char *grn, unsigned char *blu,
	unsigned char *nul)
{
	if (driver->Scaled_raster)
		return (*driver->Scaled_raster)(n, row, red, grn, blu, nul);

	return -1;
}

