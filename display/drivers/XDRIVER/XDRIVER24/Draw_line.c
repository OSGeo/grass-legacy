#include "includes.h"
/* draw a line between two given points in the current color. X version
 * 
 * Called by: Cont_abs() in ../lib/Cont_abs.c */

extern Display *dpy;
extern Window grwin;
extern GC gc;
extern Pixmap bkupmap;
extern int backing_store;

int draw_line (int cur_x, int cur_y, int x, int y)
{
    XDrawLine(dpy, grwin, gc, cur_x, cur_y, x, y);
    if (!backing_store)
        XDrawLine(dpy, bkupmap, gc, cur_x, cur_y, x, y);

    return 0;
}
