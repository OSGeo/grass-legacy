/*  %W%  %G%  */

/*
 *   Xpoints
 *
 *   Usage:  Xpoints color=name size=num type=x/diamond/box/+
 *
 *   Draws small marks at points listed in stdin.
 */

#define USAGE1  "color=name size=num type=x/diamond/box/+"
#define USAGE2  "points, X Y, provided in stdin"
#include "gis.h"

#define MAIN
#include "options.h"

#include <stdio.h>
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

Display *dpy;
Window win;
Colormap colormap;
int scrn;
GC  gc;
double U_to_D_xconv, U_to_D_yconv;
double xl, xr, yt, yb;

main(argc, argv)
int argc;
char **argv;
{
    char buff[128];
    struct Cell_head window;
    unsigned long foregr;
    extern int stash_away();
    int i;

    Window root_return;
    int x, y;
	unsigned window_width, window_height, border_width, depth;
    XWindowAttributes xwa;


	/* Initialize the GIS calls */
    G_gisinit(argv[0]);

	/* Check command line */
    set_default_options();

    if (D_parse_command(argc, argv, variables, n_variables, stash_away)) {
        fprintf(stderr, "Usage: %s %s\n", argv[0], USAGE1);
        fprintf(stderr, "       %s\n", USAGE2);
        exit(-1);
    }
    /* Set the display to be the default display */
    if (!(dpy = XOpenDisplay(NULL))) {
        fprintf(stderr, " can't open display\n");
        exit(-1);
    }
    scrn = DefaultScreen(dpy);
    win = XD_get_cur_window(dpy, scrn);
    gc = XCreateGC(dpy, win, 0, None);

    XGetGeometry(dpy, win, &root_return, &x, &y, &window_width,
        &window_height, &border_width, &depth);

    XGetWindowAttributes(dpy, win, &xwa);
    colormap = xwa.colormap;

    foregr = XD_make_colr(dpy, win, scrn, colormap, color);
    XSetForeground(dpy, gc, foregr);


/* Setup driver and check important information */
/*
    if (D_get_cur_wind(window_name))
        G_fatal_error("No current window") ;

    if (D_set_cur_wind(window_name))
        G_fatal_error("Current window not available") ;
*/

/* Read in the map window associated with window */
    G_get_window(&window);

    XD_get_screen_bounds(&xl, &xr, &yt, &yb,
        window.north, window.south, window.west,
        window.east, window_width, window_height);

    U_to_D_xconv = (xr - xl) /
        (window.east - window.west);

/* printf("\n xr = %lf, xl = %lf, east = %lf, west = %lf",
    xr, xl, window.east, window.west);
*/

    U_to_D_yconv = (yb - yt) / (window.north - window.south);

/*
    if (D_check_map_window(&window))
        G_fatal_error("Setting map window") ;

    if (G_set_window(&window) == -1)
        G_fatal_error("Current window not settable") ;
*/


	/* Do the plotting */
    switch (type) {
    case TYPE_X:
        draw_points_x(size, &window);
        break;
    case TYPE_PLUS:
        draw_points_plus(size, &window);
        break;
    case TYPE_BOX:
        draw_points_box(size, &window);
        break;
    case TYPE_DIAMOND:
        draw_points_diamond(size, &window);
        break;
    }

    XFlush(dpy);

    if (infile != stdin) {
        /* Add this command to list */
        strcpy(buff, argv[0]);
        for (i = 1; i < argc; i++) {
            strcat(buff, " ");
            strcat(buff, argv[i]);
        }
    }
}
