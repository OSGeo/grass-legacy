/* %W%  %G%  */
#include "gis.h"
#define MAIN
#include "options.h"
#include <X11/Xlib.h>
#include <X11/Xutil.h>

Window win;
Display *dpy;
GC  gc;
XGCValues gc_values;
int screen;

/* 3d program gathers input for D3d which is then executed */

main(argc, argv)
int argc;
char **argv;
{
    int do_it, do_erase;

    G_gisinit(argv[0]);
    G_get_window(&window);

    if (!(dpy = XOpenDisplay(NULL)))
        G_fatal_error(" Can't open display\n");
    screen = DefaultScreen(dpy);
    win = XD_get_cur_window(dpy, screen);
    gc = XCreateGC(dpy, win, None, &gc_values);

    for (;;) {
        get_inputs(&do_it, &do_erase);
        if (!do_it)
            break;

        if (check_options())
            G_fatal_error("Inappropriate 3d request");

        if (0 > G_set_window(&window))
            G_fatal_error("Inappropriate window resolution request");
        if (do_erase)
            XClearWindow(dpy, win);
        establish_view(from_easting, from_northing, from_height,
            to_easting, to_northing, to_height, field);

        threed(1);
    }
}

