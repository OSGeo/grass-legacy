/*
*   Xlegend
*   Usage:  Xlegend mapname [color]
*           Xlegend name=mapname color=name type=type lines=nlines
*   Print a legend for a map
*/

#include <stdio.h>
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "gis.h"
#define MAIN
#include "options.h"
#include "driver.h"

#define USAGE   "name=mapname [color=name]"

Display *dpy;
Window win;
int scrn;
GC  boxgc, fillgc, gc;
XGCValues values;
extern int stash_away();

main(argc, argv)
int argc;
char **argv;
{
    char buff[256], *mapset;
	char *XD_colormode();
    struct Categories cats;
    struct Range range;
    struct Colors colors;
    u_long pixel, *lookup_tbl;
    int i, x, y;
    int ncats, do_cats, float_flag = 0;
    int cur_dot_row, dots_per_line;
    XWindowAttributes xwa;
    unsigned int window_width, window_height, border_width, depth;
    Window root_return;
    Colormap cmap;

    /* Initialize the GIS calls */
    G_gisinit(argv[0]);

    /* Check command line */
    set_default_options();

    if (D_parse_command(argc, argv, variables, n_variables, stash_away)) {
        fprintf(stderr, "Usage: %s %s\n", argv[0], USAGE);
        exit(-1);
    }
    if (!strlen(map_name)) {
        fprintf(stderr, "Usage: %s %s\n", argv[0], USAGE);
        exit(-1);
    }
    /* Make sure map is available */
    mapset = G_find_cell(map_name, "");
    if (mapset == NULL) {
        (void)sprintf(buff, "Cellfile [%s] not available", map_name);
        G_fatal_error(buff);
    }
    /* Set the display to be the default display */
    if (!(dpy = XOpenDisplay(NULL))) {
        fprintf(stderr, "%s: can't open display\n", argv[0]);
        exit(-1);
    }
    scrn = DefaultScreen(dpy);
    if (!(win = XD_get_cur_window(dpy, scrn))) {
        fprintf(stderr, "xcell: bad window id\n");
        exit(-1);
    }
    boxgc = XCreateGC(dpy, win, 0, None);
    fillgc = XCreateGC(dpy, win, 0, None);
    gc = XCreateGC(dpy, win, 0, None);
    XGetGeometry(dpy, win, &root_return, &x, &y, &window_width,
        &window_height, &border_width, &depth);
    XGetWindowAttributes(dpy, win, &xwa);
    cmap = xwa.colormap;
    init_font("romand");

    if (G_read_colors(map_name, mapset, &colors) == -1) {
        (void)sprintf(buff, "color file for [%s] not available", map_name);
        G_fatal_error(buff);
    }
    if (G_read_cats(map_name, mapset, &cats) == -1) {
        (void)sprintf(buff, "Category file for [%s] not available", map_name);
        G_fatal_error(buff);
    }
    if (G_read_range(map_name, mapset, &range) == -1) {
        (void)sprintf(buff, "Range information for [%s] not available", map_name);
        G_fatal_error(buff);
    }
    /* How many categories to show */
    ncats = range.pmax - range.nmin + 1;
    do_cats = (ncats + 1) < lines ? ncats : lines - 1;

    lookup_tbl = (u_long *)calloc(ncats, sizeof(u_long));
    for (i=0; i < ncats; i++) {
        lookup_tbl[i] = XD_calc_closest_colr(cmap,
                colors.red[i] * colors.red[i],
                colors.grn[i] * colors.grn[i],
                colors.blu[i] * colors.blu[i]);
    }
	if (strcmp("float", XD_colormode(dpy, win, scrn)) == 0)
		float_flag = 1;
    
    /* Figure number of lines, number of pixles per line
    *  and text size */
    if (lines == 0)
        lines = ncats + 1;
    dots_per_line = window_height / lines;

    Text_size((int)(dots_per_line * 4 /5), (int)(dots_per_line * 4 /5));
    cur_dot_row = dots_per_line / 2;

    XSetForeground(dpy, boxgc, WhitePixel(dpy, scrn));
    if ((pixel = XD_make_colr(dpy, win, scrn, cmap, color)))
        XSetForeground(dpy, gc, pixel);
    else
        XSetForeground(dpy, gc, BlackPixel(dpy, scrn));

    for (i = range.nmax; i <= range.pmax; i++) {
		if (float_flag)
			XSetForeground(dpy, fillgc, i % 216 + 38);
		else
        	XSetForeground(dpy, fillgc, lookup_tbl[i - colors.min]);
        XFillRectangle(dpy, win, fillgc, 2, cur_dot_row,
                dots_per_line-2, dots_per_line-2);

        /* White box */
        XDrawRectangle(dpy, win, boxgc, 2, cur_dot_row,
                dots_per_line-2, dots_per_line-2);

        cur_dot_row += dots_per_line;
        /* Draw text */
        (void)sprintf(buff, "%2d) %s", i, G_get_cat(i, &cats));
        Move_abs((3 + dots_per_line), cur_dot_row - dots_per_line/4);
        Text(buff);
    }
    XFlush(dpy);
}

