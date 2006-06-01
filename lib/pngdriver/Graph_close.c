/*
 * Close down the graphics processing.  This gets called only at driver
 * termination time.
 */

#include <grass/gis.h>
#include "pngdriver.h"

void PNG_Graph_close(void)
{
	write_image();
	G_free(grid);
}

