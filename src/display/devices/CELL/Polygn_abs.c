/*
 * A polygon is drawn using the current color.  It has "number" verticies
 * which are found in the absolute coordinate pairs represented in the
 * "xarray" and "yarray" arrays.
 */

#include "cell.h"

extern int horiz_line();

int Polygon_abs (int *x, int *y, int n)
{
    polyfill (x, y, n, horiz_line);
    return 0;
}
