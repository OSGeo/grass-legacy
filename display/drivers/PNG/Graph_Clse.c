/*
 * Close down the graphics processing.  This gets called only at driver
 * termination time.
 */

#include "gis.h"
#include "pngdriver.h"

int
Graph_Close(void)
{
	write_image();
	G_free(grid);

	return 0;
}

