/*
 * A polygon is drawn using the current color.  It has "number" verticies
 * which are found in the absolute coordinate pairs represented in the
 * "xarray" and "yarray" arrays.
 */

#include <gd.h>
#include "png.h"
#include "gis.h"

Polygon_abs(int *xarray, int *yarray, int number)
{
	static gdPointPtr points;
	static int size;
	int i;

	if (size < number)
	{
		size = number;
		points = G_realloc(points, sizeof(gdPoint) * size);
	}

	for (i = 0; i < number; i++)
	{
		points[i].x = xarray[i];
		points[i].y = yarray[i];
	}

	gdImageFilledPolygon(im, points, number, currentColor);
}
