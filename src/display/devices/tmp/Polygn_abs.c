/*
 * A polygon is drawn using the current color.  It has "number" verticies
 * which are found in the absolute coordinate pairs represented in the
 * "xarray" and "yarray" arrays.
 *
 *  Written by the GRASS Team in the Winter of 88.
 *
 */


static fillrout ();

Polygon_abs(xarray, yarray, number)
	int *xarray, *yarray ;
	int number ;
{

    polyfill (xarray, yarray, number, fillrout);
}

static 
fillrout (row, x1, x2)
    int row, x1, x2;
{
	draw_line (x1, row, x2, row);
}
