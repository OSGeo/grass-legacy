#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xatom.h>
#include "gis.h"

double e_minus_w, n_minus_s;
struct Cell_head window;
extern Display *dpy;
extern Window display_win;
char n_str[12], s_str[12], w_str[12], e_str[12], nsres_str[12], ewres_str[12];
Atom north, south, east, west, ns, ew;

void attach_window_info()
{
    void set_win_info();

    G_gisinit("junk");
    G_get_window(&window);

    sprintf(n_str, "%10.2lf", window.north);
    sprintf(s_str, "%10.2lf", window.south);
    sprintf(w_str, "%10.2lf", window.west);
    sprintf(e_str, "%10.2lf", window.east);
    sprintf(nsres_str, "%10.2lf", window.ns_res);
    sprintf(ewres_str, "%10.2lf", window.ew_res);

    north = XInternAtom(dpy, "grass_north", False);
    south = XInternAtom(dpy, "grass_south", False);
    west = XInternAtom(dpy, "grass_west", False);
    east = XInternAtom(dpy, "grass_east", False);
    ns = XInternAtom(dpy, "grass_nsres", False);
    ew = XInternAtom(dpy, "grass_ewres", False);
    set_win_info();
}


void set_win_info()
{
    XChangeProperty(dpy, display_win, north, XA_STRING, 8, 
		PropModeReplace, (u_char*)n_str, 12);
    XChangeProperty(dpy, display_win, south, XA_STRING, 8,
		PropModeReplace, (u_char*)s_str, 12);
    XChangeProperty(dpy, display_win, west, XA_STRING, 8,
		PropModeReplace, (u_char*)w_str, 12);
    XChangeProperty(dpy, display_win, east, XA_STRING, 8,
		PropModeReplace, (u_char*)e_str, 12);
    XChangeProperty(dpy, display_win, ns, XA_STRING, 8,
		PropModeReplace, (u_char*)nsres_str, 12);
    XChangeProperty(dpy, display_win, ew, XA_STRING, 8,
		PropModeReplace, (u_char*)ewres_str, 12);
    sscanf(n_str, "%lf", &window.north);
    sscanf(s_str, "%lf", &window.south);
    sscanf(w_str, "%lf", &window.west);
    sscanf(e_str, "%lf", &window.east);
    sscanf(nsres_str, "%lf", &window.ns_res);
    sscanf(ewres_str, "%lf", &window.ew_res);
    e_minus_w = window.east - window.west;
    n_minus_s = window.north - window.south;
    XFlush(dpy);
}

