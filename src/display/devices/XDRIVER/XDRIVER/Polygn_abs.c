#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "../lib/driver.h"

/* A polygon is drawn using the current color.  It has "number"
 * verticies which are found in the absolute coordinate pairs
 * represented in the "xarray" and "yarray" arrays.  NOTE: Cursor
 * location is NOT updated in Polygon_rel(). */

extern Display *dpy;
extern Window grwin;
extern GC gc;
extern Pixmap bkupmap;
extern int backing_store;

XPoint *AllocXPoints();

Polygon_abs(xarray, yarray, number)
int *xarray, *yarray, number;
{
    register i;
    register XPoint *xpnts;

    /* The two separate x and y coord arrays must be combined for X.
     * First allocate space for the XPoint struct. */
    xpnts = AllocXPoints(number);
    /* now move coordinate pairs together */
    for (i = 0; i < number; i++) {
        xpnts[i].x = (short) xarray[i];
        xpnts[i].y = (short) yarray[i];
    }
    XFillPolygon(dpy, grwin, gc, xpnts, number, Complex,
            CoordModeOrigin);
    if (backing_store != Always)
        XFillPolygon(dpy, bkupmap, gc, xpnts, number, Complex,
                CoordModeOrigin);
    return 1;
}


Polygon_rel(xarray, yarray, number)
int *xarray, *yarray, number;
{
    register i = 0;
    register XPoint *xpnts;

    xpnts = AllocXPoints(number);
    xpnts[i].x = (short) (xarray[i] + cur_x);
    xpnts[i].y = (short) (yarray[i] + cur_y);
    for (i = 1; i < number; i++) {
        xpnts[i].x = (short) xarray[i];
        xpnts[i].y = (short) yarray[i];
    }
    XFillPolygon(dpy, grwin, gc, xpnts, number, Complex,
            CoordModePrevious);
    if (backing_store != Always)
        XFillPolygon(dpy, bkupmap, gc, xpnts, number, Complex,
                CoordModePrevious);
    return 1;
}

/*** end Polygon_abs.c ***/
