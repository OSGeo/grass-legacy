#include "includes.h"
#include "../lib/driver.h"

extern Display *dpy;
extern Window grwin;
extern GC gc;
extern Pixmap bkupmap;
extern int backing_store;


int Cont_abs (int x, int y)
{
    XDrawLine(dpy, grwin, gc, cur_x, cur_y, x, y);
    if (!backing_store)
        XDrawLine(dpy, bkupmap, gc, cur_x, cur_y, x, y);
    cur_x = x;
    cur_y = y;

    return 0;
}

int Cont_rel (int x, int y)
{
    XDrawLine(dpy, grwin, gc, cur_x, cur_y, cur_x + x, cur_y + y);
    if (!backing_store)
        XDrawLine(dpy, bkupmap, gc, cur_x, cur_y, cur_x + x, cur_y + y);
    cur_x += x;
    cur_y += y;

    return 0;
}

/*** end cont_abs.c ***/
