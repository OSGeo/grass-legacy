#include <stdio.h>
#include <X11/Xos.h)
#include <X11/Xlib.h>
#include <X11/Xutil.h>

#define START_X 10
#define START_Y 20
#define WINDOW_WIDTH 700
#define WINDOW_HEIGHT 550
#define BORDER_WIDTH 1
#define KEY_STR_LENGTH 20

Display *dpy;
Window win;
int scrn;
Window root_window;
XSizeHints size_hints;
GC  gc;
XGCValues gc_values;
unsigned long gc_valuemask;
unsigned int mask;

create_Xwindow()
{
    if (!(dpy = XOpenDisplay(NULL)))
        G_fatal_error(" Can't open display\n");
    scrn = DefaultScreen(dpy);
    root_window = RootWindow(dpy, scrn);

    size_hints.x = START_X;
    size_hints.y = START_Y;
    size_hints.width = WINDOW_WIDTH;
    size_hints.height = WINDOW_HEIGHT;
    size_hints.flags = PSize | PPosition;

    if ((win = XCreateSimpleWindow(dpy, root_window,
                START_X, START_Y, WINDOW_WIDTH,
                WINDOW_HEIGHT, BORDER_WIDTH,
                BlackPixel(dpy, scrn),
                WhitePixel(dpy, scrn))) == 0) {
        G_fatal_error(" Window open failed\n");
    }
    XSetStandardProperties(dpy, win, "My Window", "My Icon",
        None, NULL, NULL, &size_hints);

    XMapWindow(dpy, win);
    XSelectInput(dpy, win, ExposureMask);
    gc = XCreateGC(dpy, win, None, &gc_values);
}
