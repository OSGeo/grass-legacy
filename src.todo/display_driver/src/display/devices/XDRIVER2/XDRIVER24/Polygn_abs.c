#include "includes.h"
#include "../lib/driver.h"
#include "layers.h"

/* A polygon is drawn using the current color.  It has "number"
 * verticies which are found in the absolute coordinate pairs
 * represented in the "xarray" and "yarray" arrays.  NOTE: Cursor
 * location is NOT updated in Polygon_rel(). */

extern Display *dpy;
extern Window grwin;
extern GC gc;
extern Pixmap bkupmap;
extern int backing_store;

int Polygon_abs (int *xarray, int *yarray, int number)
{
    register int i;
    register XPoint *xpnts;
	layer_t *s;
	s=get_scratch_layer();
	s->flags |= TRANSPARENT;

    /* The two separate x and y coord arrays must be combined for X.
     * First allocate space for the XPoint struct. */
    xpnts = AllocXPoints(number);
    /* now move coordinate pairs together */
    for (i = 0; i < number; i++) {
        xpnts[i].x = (short) xarray[i];
        xpnts[i].y = (short) yarray[i];
    }
    XFillPolygon(dpy, s->l, s->gc, xpnts, number, Complex,
            CoordModeOrigin);
    XFillPolygon(dpy, s->m, s->mgc, xpnts, number, Complex,
            CoordModeOrigin);
/*      XFillPolygon(dpy, grwin, gc, xpnts, number, Complex, */
/*              CoordModeOrigin); */
    if (!backing_store)
        XFillPolygon(dpy, bkupmap, gc, xpnts, number, Complex,
                CoordModeOrigin);
    return 1;
}

int Polygon_rel (int *xarray, int *yarray, int number)
{
    register int i = 0;
    register XPoint *xpnts;
	layer_t *s;
	s=get_scratch_layer();
	s->flags |= TRANSPARENT;

    xpnts = AllocXPoints(number);
    xpnts[i].x = (short) (xarray[i] + cur_x);
    xpnts[i].y = (short) (yarray[i] + cur_y);
    for (i = 1; i < number; i++) {
        xpnts[i].x = (short) xarray[i];
        xpnts[i].y = (short) yarray[i];
    }
    XFillPolygon(dpy, s->l, s->gc, xpnts, number, Complex,
            CoordModePrevious);
    XFillPolygon(dpy, s->m, s->mgc, xpnts, number, Complex,
            CoordModePrevious);
/*      XFillPolygon(dpy, grwin, gc, xpnts, number, Complex, */
/*              CoordModePrevious); */
    if (!backing_store)
        XFillPolygon(dpy, bkupmap, gc, xpnts, number, Complex,
                CoordModePrevious);
    return 1;
}

/*** end Polygon_abs.c ***/
