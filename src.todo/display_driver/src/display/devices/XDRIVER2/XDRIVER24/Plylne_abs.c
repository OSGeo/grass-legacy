#include <stdio.h>
#include <stdlib.h>
#include "includes.h"
#include "../lib/driver.h"
#include "layers.h"

extern Display *dpy;
extern Window grwin;
extern GC gc;
extern Pixmap bkupmap;

int Polyline_abs (int *xarray, int *yarray, int number)
{
    int i;
    XPoint *xpnts = NULL;
	layer_t *s;

    xpnts = AllocXPoints(number);
    for (i = 0; i < number; i++) {
        xpnts[i].x = (short) xarray[i];
        xpnts[i].y = (short) yarray[i];
    }

	s=get_scratch_layer();
	s->flags |= TRANSPARENT;
    XDrawLines(dpy, s->l, s->gc, xpnts, number, CoordModeOrigin);
    XDrawLines(dpy, s->m, s->mgc, xpnts, number, CoordModeOrigin);
/*     XDrawLines(dpy, grwin, gc, xpnts, number, CoordModeOrigin); */
    cur_x = xarray[number - 1];
    cur_y = yarray[number - 1];

    return 0;
}

int Polyline_rel (int *xarray, int *yarray, int number)
{
    register int i = 0;
    XPoint *xpnts = NULL;
	layer_t *s;

    xpnts = AllocXPoints(number);
    xpnts[i].x = (short) (xarray[i] + cur_x);
    xpnts[i].y = (short) (yarray[i] + cur_y);
    for (i = 1; i < number; i++) {
        xpnts[i].x = (short) xarray[i];
        xpnts[i].y = (short) yarray[i];
    }
/*      XDrawLines(dpy, grwin, gc, xpnts, number, CoordModePrevious); */
	s=get_scratch_layer();
	s->flags |= TRANSPARENT;
    XDrawLines(dpy, s->l, s->gc, xpnts, number, CoordModeOrigin);
    XDrawLines(dpy, s->m, s->mgc, xpnts, number, CoordModeOrigin);

    return 0;
}
