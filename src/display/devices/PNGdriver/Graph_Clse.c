/*
 * Close down the graphics processing.  This gets called only at driver
 * termination time.
 */

#include "png.h"
#include "gis.h"
#include "driverlib.h"


int Graph_Close(void)
{
#if defined(HAVE_GDIMAGEPNG)
	gdImagePng(im, output);
#elif defined(HAVE_GDIMAGEGIF)
	gdImageGif(im, output);
#else
# error Neither PNG nor GIF supported
#endif
	fclose(output);
	gdImageDestroy(im);
}
