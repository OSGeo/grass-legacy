#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "../lib/driver.h"

extern Display *dpy;
extern Window grwin;
extern GC gc;
extern Pixmap bkupmap;
extern int backing_store;


Cont_abs(x, y)
int x, y;
{
    XDrawLine(dpy, grwin, gc, cur_x, cur_y, x, y);
    if (backing_store != Always)
        XDrawLine(dpy, bkupmap, gc, cur_x, cur_y, x, y);
    cur_x = x;
    cur_y = y;
}


Cont_rel(x, y)
int x, y;
{
    XDrawLine(dpy, grwin, gc, cur_x, cur_y, cur_x + x, cur_y + y);
    if (backing_store != Always)
        XDrawLine(dpy, bkupmap, gc, cur_x, cur_y, cur_x + x, cur_y + y);
    cur_x += x;
    cur_y += y;
}

/*** end cont_abs.c ***/
