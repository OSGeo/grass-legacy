/* Close down the graphics processing.  This gets called only at driver
 * termination time. */
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

Graph_Close()
{
    extern Display *dpy;
    extern Window grwin;

    XDestroyWindow(dpy, grwin);
    return (0);
}
