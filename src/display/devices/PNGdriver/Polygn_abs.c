/*
 * A polygon is drawn using the current color.  It has "number" verticies
 * which are found in the absolute coordinate pairs represented in the
 * "xarray" and "yarray" arrays.
 */

#include <gd.h>
#include "png.h"

Polygon_abs(xarray, yarray, number)
	int *xarray, *yarray ;
	int number ;
{
	gdPointPtr points;
	int i;
	int red;

	points = (gdPointPtr)G_malloc(sizeof(gdPoint)*number);
	for(i=0;i<number;i++) {
		(*(points+i)).x = xarray[i];
		(*(points+i)).y = yarray[i];
	}
	gdImageFilledPolygon(im, points, number, currentColor);
}
