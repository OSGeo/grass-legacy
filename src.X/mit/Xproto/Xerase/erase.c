/*
 *   Xerase
 *   Usage:  Xerase color
 *   Erase a window.  This erases the memory of what was on the window.
 */

#include <stdio.h>
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>

#define USAGE   "[color]"

Display *dpy;
Window win;
int scrn;
GC  gc;

char c_name[32], o_name[32], v_name[32];

main(argc, argv)
int argc;
char **argv;
{
    Atom property ;
    u_long background;
    Window root_return;
    XWindowAttributes xwa;
	int x, y;
    unsigned window_width, window_height, border_width, depth;

    /* Initialize the GIS calls */
    G_gisinit("Xerase");

    /* Set the display to be the default display */
    if (!(dpy = XOpenDisplay(NULL))) {
        fprintf(stderr, "can't open display\n");
        exit(-1);
    }
    scrn = DefaultScreen(dpy);
    win = XD_get_cur_window(dpy, scrn);
    XGetWindowAttributes(dpy, win, &xwa);

    /* Check command line */
    if (argc > 2) {
        fprintf(stderr, "Usage: %s %s\n", argv[0], USAGE);
        exit(-1);
    }
    /* Decipher color */
    if (argc == 2)
        background = XD_make_colr(dpy, win, scrn, xwa.colormap,
				argv[1]);
    else
        background = BlackPixel(dpy, scrn);

    XGetGeometry(dpy, win, &root_return, &x, &y, &window_width,
        &window_height, &border_width, &depth);

    XSetWindowBackground(dpy, win, background);
    XClearWindow(dpy, win);

    strcpy(c_name, "None");
    if ((property = XInternAtom(dpy, "grass_cell", True)) != None)
    	XChangeProperty(dpy, win, property, XA_STRING, 8,
				PropModeReplace, (u_char*)c_name, strlen(c_name));
    if ((property = XInternAtom(dpy, "grass_vect", True)) != None)
    	XChangeProperty(dpy, win, property, XA_STRING, 8,
				PropModeReplace, (u_char*)c_name, strlen(c_name));
    if ((property = XInternAtom(dpy, "grass_overlay", True)) != None)
    	XChangeProperty(dpy, win, property, XA_STRING, 8,
				PropModeReplace, (u_char*)c_name, strlen(c_name));
    XFlush(dpy);
	exit(0);
}

