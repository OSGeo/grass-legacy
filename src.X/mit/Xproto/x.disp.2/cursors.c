#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/StringDefs.h>
#include <X11/cursorfont.h>

Cursor cur1, cross_hair;
extern Display *dpy;

create_cursors()
{
    cur1 = XCreateFontCursor(dpy, XC_pencil);
    cross_hair = XCreateFontCursor(dpy, XC_crosshair);
}

