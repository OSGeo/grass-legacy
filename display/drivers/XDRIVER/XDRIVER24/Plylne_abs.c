#include <stdio.h>
#include <stdlib.h>
#include "includes.h"
#include "driver.h"

int Polyline_abs (int *xarray, int *yarray, int number)
{
    int i;
    XPoint *xpnts = NULL;

    xpnts = AllocXPoints(number);
    for (i = 0; i < number; i++) {
        xpnts[i].x = (short) xarray[i];
        xpnts[i].y = (short) yarray[i];
    }
    XDrawLines(dpy, bkupmap, gc, xpnts, number, CoordModeOrigin);
    cur_x = xarray[number - 1];
    cur_y = yarray[number - 1];
    needs_flush = 1;

    return 0;
}

int Polyline_rel (int *xarray, int *yarray, int number)
{
    register int i = 0;
    XPoint *xpnts = NULL;

    xpnts = AllocXPoints(number);
    xpnts[i].x = (short) (xarray[i] + cur_x);
    xpnts[i].y = (short) (yarray[i] + cur_y);
    for (i = 1; i < number; i++) {
        xpnts[i].x = (short) xarray[i];
        xpnts[i].y = (short) yarray[i];
    }
    XDrawLines(dpy, bkupmap, gc, xpnts, number, CoordModePrevious);
    needs_flush = 1;

    return 0;
}
