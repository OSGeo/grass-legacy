/*
 * A polygon is drawn using the current color.  It has "number" verticies
 * which are found in the absolute coordinate pairs represented in the
 * "xarray" and "yarray" arrays.
 */

#include "graphics.h"
#define NULLPR  (Pixrect *) NULL

Polygon_abs(xarray, yarray, number)
        int *xarray, *yarray ;
        int number ;
{
        int *yptr, *xptr;
        struct pr_pos pts[1024];
        int num[1], i;

        xptr = xarray;
        yptr = yarray;
        for(i=0; i < number; i++) {
                pts[i].x = *xptr++;
                pts[i].y = *yptr++;
        }

        num[0] = number;
        pw_polygon_2(pixwin, 0, 0, 1, num, pts, Op, NULLPR, 0, 0) ;
}
