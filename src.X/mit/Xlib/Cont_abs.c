/* %W% %G% */

#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

extern Display *dpy;
extern Window win;
extern GC gc;

extern int cur_x, cur_y;

Cont_abs(x, y)
int x, y;
{
    XDrawLine(dpy, win, gc, cur_x, cur_y, x, y);
    cur_x = x;
    cur_y = y;
}

