/*  %W%  %G%  */
/*   Xscale
 *   Usage:  Xscale color1 color2
 *           Xscale color1=name color2=name
 *   Draw an "appropriate" scale on the map
 */


#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "gis.h"
#define MAIN
#include "options.h"
#include "driver.h"

#define NUMSCALES   10
#define USAGE   "[color1=name] [color2=name]"

Display *dpy;
Window win;
Colormap cmap;
int scrn;
GC  gc;
XGCValues values;


/* declare variables */
static struct {
    char *name;
    double size;
    double limit;
} scales[] = {
    "", 0., 20.,
    "10 meters", 10.0, 70.0,
    "50 meters", 50.0, 200.0,
    "100 meters", 100.0, 700.0,
    "500 meters", 500.0, 2000.0,
    "1 km", 1000.0, 7000.0,
    "5 km", 5000.0, 20000.0,
    "10 km", 10000.0, 70000.0,
    "50 km", 50000.0, 200000.0,
    "100 km", 100000.0, 700000.0
};

struct Cell_head window;

main(argc, argv)
int argc;
char **argv;
{
    extern int stash_away();

	/* Initialize the GIS calls */
    G_gisinit(argv[0]);

	/* Check command line */
    set_default_options();

    if (D_parse_command(argc, argv, variables, n_variables, stash_away)) {
        fprintf(stderr, "Usage: %s %s\n", argv[0], USAGE);
        exit(-1);
    }

    G_get_window(&window);

	/* Draw the scale */
    draw_scale(color1, color2);

    XFlush(dpy);
}


draw_scale(color1, color2)
char *color1, *color2;
{
    u_long colr1, colr2;
    double meters;
    int line_len;
    int incr;
    double XD_get_a_to_d_xconv();
    double XD_get_u_east(), XD_get_u_west();
    double XD_get_d_west(), XD_get_d_north();
    int D_west, D_north;
    double XD_get_ew_resolution();
    int i, r;
    int size;
    XWindowAttributes xwa;
    Window root_return;
    int x, y;
	unsigned window_width, window_height, border_width, depth;

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

    if (XD_do_conversions(&window, window_width, window_height))
        G_fatal_error("Error in calculating conversions");

    XGetWindowAttributes(dpy, win, &xwa);
    cmap = xwa.colormap;
    colr1 = XD_make_colr(dpy, win, scrn, cmap, color1);
    colr2 = XD_make_colr(dpy, win, scrn, cmap, color2);
    init_font("romand");

	/* Establish text size */
    size = (int) (.025 * (float) window_height);
    Text_size(size, size);

    meters = XD_get_u_east() - XD_get_u_west();
    D_north = (int) XD_get_d_north();
    D_west = (int) XD_get_d_west();

	/* find the right scale */
    for (incr = 0; incr < NUMSCALES; incr++) {
        if (meters <= scales[incr].limit)
            break;
    }

    if (!incr)
        return (-1);

    line_len = (int)(XD_get_a_to_d_xconv() * (scales[incr].size / XD_get_ew_resolution()));

	/* Blank out area with background color */
    XSetForeground(dpy, gc, colr1);
    r = D_west + 35 + line_len + size * strlen(scales[incr].name);
    for (i = D_north + 5; i < D_north + 35; i++)
        Move_abs(D_west + 5, i), Cont_abs(r, i);

	/* Draw legend */
    XSetForeground(dpy, gc, colr2);
    Move_abs(D_west + 10, D_north + 25);
    Cont_rel(0, -10);
    Cont_rel(10, 10);
    Cont_rel(0, -10);
    Move_rel(-5, 14);
    Cont_rel(0, -17);
    Cont_rel(-2, -0);
    Cont_rel(2, -2);
    Cont_rel(2, 2);
    Cont_rel(-2, -0);
    Move_abs(D_west + 30, D_north + 10);
    Cont_abs(D_west + 30, D_north + 30);
    Move_abs(D_west + 30, D_north + 20);
    Cont_abs(D_west + 30 + line_len, D_north + 20);
    Move_abs(D_west + 30 + line_len, D_north + 10);
    Cont_abs(D_west + 30 + line_len, D_north + 30);

    Move_abs(D_west + 40 + line_len, D_north + 25);
    Text(scales[incr].name);
    return (0);
}

