/* Close down the graphics processing.  This gets called only at driver
 * termination time. */

#include "includes.h"

int Graph_Close (void)
{
    extern Display *dpy;
    extern Window grwin;

    XDestroyWindow(dpy, grwin);
    return (0);
}
