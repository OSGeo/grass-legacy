#include "includes.h"
/* draw a point in the current color. X version */

int draw_point (int x,int y)
{
    XDrawPoint(dpy, bkupmap, gc, x, y);
    needs_flush = 1;

    return 0;
}
