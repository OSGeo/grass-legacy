#include "includes.h"
/* draw a line between two given points in the current color. X version */

int draw_line (int cur_x, int cur_y, int x, int y)
{
    XDrawLine(dpy, bkupmap, gc, cur_x, cur_y, x, y);
    needs_flush = 1;

    return 0;
}
