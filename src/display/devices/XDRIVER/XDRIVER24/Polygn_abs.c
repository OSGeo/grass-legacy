#include "includes.h"
#include "driver.h"

/* A polygon is drawn using the current color.  It has "number"
 * verticies which are found in the absolute coordinate pairs
 * represented in the "xarray" and "yarray" arrays.  NOTE: Cursor
 * location is NOT updated in Polygon_rel(). */

int Polygon_abs (int *xarray, int *yarray, int number)
{
    register int i;
    register XPoint *xpnts;

    /* The two separate x and y coord arrays must be combined for X.
     * First allocate space for the XPoint struct. */
    xpnts = AllocXPoints(number);
    /* now move coordinate pairs together */
    for (i = 0; i < number; i++) {
        xpnts[i].x = (short) xarray[i];
        xpnts[i].y = (short) yarray[i];
    }
    XFillPolygon(dpy, bkupmap, gc, xpnts, number, Complex, CoordModeOrigin);
    needs_flush = 1;
    return 1;
}

int Polygon_rel (int *xarray, int *yarray, int number)
{
    register int i = 0;
    register XPoint *xpnts;

    xpnts = AllocXPoints(number);
    xpnts[i].x = (short) (xarray[i] + cur_x);
    xpnts[i].y = (short) (yarray[i] + cur_y);
    for (i = 1; i < number; i++) {
        xpnts[i].x = (short) xarray[i];
        xpnts[i].y = (short) yarray[i];
    }
    XFillPolygon(dpy, bkupmap, gc, xpnts, number, Complex, CoordModePrevious);
    needs_flush = 1;
    return 1;
}

/*** end Polygon_abs.c ***/
