/*
 * Close down the graphics processing.  This gets called only at driver
 * termination time.
 */

#include "png.h"
#include "gis.h"
#include "driverlib.h"


int Graph_Close(void)
{
	gdImagePng(im, output);
	fclose(output);
	gdImageDestroy(im);
}
