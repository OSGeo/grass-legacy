#include <X11/Xatom.h>
#include <X11/Xlib.h>




XD_get_window(dpy, win, n, s, w, e, ns, ew)
Display *dpy;
Window win;
double *n, *s, *w, *e, *ns, *ew;
{

    Atom north, south, west, east, ew_res, ns_res;
    unsigned char *one, *two, *three, *four, *five, *six;
    unsigned long nitems, remaining;
    int actual_format;
    Atom actual_type;


    north = XInternAtom(dpy, "north", True);
    south = XInternAtom(dpy, "south", True);
    west = XInternAtom(dpy, "west", True);
    east = XInternAtom(dpy, "east", True);
    ns_res = XInternAtom(dpy, "nsres", True);
    ew_res = XInternAtom(dpy, "ewres", True);

    XGetWindowProperty(dpy, win, north,
        0, 32, False, AnyPropertyType, &actual_type,
        &actual_format, &nitems, &remaining, &one);

    XGetWindowProperty(dpy, win, south,
        0, 32, False, AnyPropertyType, &actual_type,
        &actual_format, &nitems, &remaining, &two);

    XGetWindowProperty(dpy, win, west,
        0, 32, False, AnyPropertyType, &actual_type,
        &actual_format, &nitems, &remaining, &three);

    XGetWindowProperty(dpy, win, east,
        0, 32, False, AnyPropertyType, &actual_type,
        &actual_format, &nitems, &remaining, &four);

    XGetWindowProperty(dpy, win, ns_res,
        0, 32, False, AnyPropertyType, &actual_type,
        &actual_format, &nitems, &remaining, &five);

    XGetWindowProperty(dpy, win, ew_res,
        0, 32, False, AnyPropertyType, &actual_type,
        &actual_format, &nitems, &remaining, &six);



    sscanf(one, "%lf", n);
    sscanf(two, "%lf", s);
    sscanf(three, "%lf", w);
    sscanf(four, "%lf", e);
    sscanf(five, "%lf", ns);
    sscanf(six, "%lf", ew);

}
