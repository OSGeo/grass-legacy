#include <stdio.h>
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>

char temp[32];
Atom property;

Window XD_get_cur_window(dpy, scrn)
Display *dpy;
int scrn;
{
    Atom actual_type;
    unsigned long nitems, remaining;
    unsigned char *one;
    int actual_format;
    unsigned long cur_window;

    if ((property = XInternAtom(dpy, "GRASS_WINDOW", True)) == None)
        fprintf(stderr, "no atom exists\n");
    XGetWindowProperty(dpy, DefaultRootWindow(dpy), property, 0L,
        1000L, False, XA_STRING, &actual_type, &actual_format,
        &nitems, &remaining, &one);
    if ((actual_type != XA_STRING) || (actual_format != 8)) {
        fprintf(stderr, "Wrong atom type\n");
        return ((Window)0);
    }
    if (sscanf(one, "%s", temp) != 1) {
        fprintf(stderr, "no window id exists\n");
        return((Window)0);
    }
    if (sscanf(temp, "0x%lx", &cur_window) != 1) {
        if ((sscanf(temp, "%lu", &cur_window)) != 1) {
        	fprintf(stderr, "Give a window ID to work with: ");
        	if ((scanf("0x%lx", &cur_window)) != 1)
                if ((scanf("%lu", &cur_window)) != 1)
                    fprintf(stderr, "no window id exists\n");
		}
    }
    /* fprintf(stderr, "cur_window = (0x%lx)\n", cur_window); */
    return(cur_window);
}
