/* Close down the graphics processing.  This gets called only at driver
 * termination time. */

#include "includes.h"

int Graph_Close (void)
{
    extern Display *dpy;
    extern Window grwin;
    extern Pixmap bkupmap;

    XFreePixmap(dpy, bkupmap);
    XDestroyWindow(dpy, grwin);
    return (0);
}
