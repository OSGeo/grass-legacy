#include <stdio.h>
#include "includes.h"
#include "../lib/driver.h"

extern Display *dpy;
extern Window grwin;
extern GC gc;
extern Pixmap bkupmap;

char *calloc(), *realloc();
XPoint *AllocXPoints();


Polyline_abs(xarray, yarray, number)
int *xarray, *yarray;
int number;
{
    int i;
    XPoint *xpnts = NULL;

    xpnts = AllocXPoints(number);
    for (i = 0; i < number; i++) {
        xpnts[i].x = (short) xarray[i];
        xpnts[i].y = (short) yarray[i];
    }
    XDrawLines(dpy, grwin, gc, xpnts, number, CoordModeOrigin);
    cur_x = xarray[number - 1];
    cur_y = yarray[number - 1];
}


Polyline_rel(xarray, yarray, number)
int *xarray, *yarray;
int number;
{
    register i = 0;
    XPoint *xpnts = NULL;

    xpnts = AllocXPoints(number);
    xpnts[i].x = (short) (xarray[i] + cur_x);
    xpnts[i].y = (short) (yarray[i] + cur_y);
    for (i = 1; i < number; i++) {
        xpnts[i].x = (short) xarray[i];
        xpnts[i].y = (short) yarray[i];
    }
    XDrawLines(dpy, grwin, gc, xpnts, number, CoordModePrevious);
}
