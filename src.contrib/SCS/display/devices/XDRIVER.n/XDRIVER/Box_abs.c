#include <stdio.h>
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "../lib/driver.h"

/*  This next line is for SYSV only */
/* typedef unsigned char u_char; */
extern Display *dpy;
extern Window grwin;
extern GC gc;
extern Pixmap bkupmap;
extern int backing_store;

Box_abs(x1, y1, x2, y2)
{
    int tmp;

    if (x1 > x2) {
        tmp = x1;
        x1 = x2;
        x2 = tmp;
    }
    if (y1 > y2) {
        tmp = y1;
        y1 = y2;
        y2 = tmp;
    }
    XFillRectangle(dpy, grwin, gc, x1, y1, (unsigned) x2 - x1 + 1,
            (unsigned) y2 - y1);
    if (backing_store != Always)
        XFillRectangle(dpy, bkupmap, gc, x1, y1, (unsigned) x2 - x1 + 1,
                (unsigned) y2 - y1);
}

/* Why I need to add 1 to the widths and heights below is a mystery.
 * Otherwise errors appear mainly when zoomed-in on cell data !? - PRT */
Box_abs2(x1, y1, width, height)
int x1, y1, width, height;
{
    XFillRectangle(dpy, grwin, gc, x1, y1, (unsigned) width + 1,
            (unsigned) height);
    if (backing_store != Always)
        XFillRectangle(dpy, bkupmap, gc, x1, y1, (unsigned) width + 1,
                (unsigned) height);
}

Box_rel(width, height)
int width, height;
{
    XFillRectangle(dpy, grwin, gc, cur_x, cur_y, (unsigned) width + 1,
            (unsigned) height);
    if (backing_store != Always)
        XFillRectangle(dpy, bkupmap, gc, cur_x, cur_y,
                (unsigned) width + 1, (unsigned) height);
}
