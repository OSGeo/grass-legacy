#include <stdio.h>
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include "gis.h"

#define MAX(x,y) ((x) > (y) ? (x) : (y))

Display *dpy;
Window win;
int scrn;
GC  gc;
Mask gc_mask;
XGCValues GC_values;
char *calloc();

Xcell(name, mapset, overlay)
char *name, *mapset;
int overlay;
{
    char buf[120];
    char *colormode, *XD_colormode();
    int float_flag = 0;
    XWindowAttributes xwa;
    struct Cell_head cellhead;
    double xl, xr, yt, yb;
    int fd, row, col, next_col, rect_y, rect_x, nrows, ncols;
    int iold, x, y, crow, ccol;
    int wid, hgt;
    unsigned width, height;
    struct Colors colors;
    register CELL *cell;
    u_long *lookup_tbl = NULL, XD_calc_closest_colr();
    Atom property;
    double delta_x, delta_y, row_inc, col_inc;
    Drawable drawable_root;
    unsigned border_width, drawable_depth;
    Colormap cmap;
    XClientMessageEvent event;

    fd = G_open_cell_old(name, mapset);
    if (fd < 0) {
        sprintf(buf, "%s in %s -can't open cell file", name, mapset);
        G_fatal_error(buf);
    }
    cell = G_allocate_cell_buf();   /* end of gis part */

    /* Set the display to be the default display */
    if (!(dpy = XOpenDisplay(NULL))) {
        fprintf(stderr, "Xcell: can't open display\n");
        return (-1);
    }
    scrn = DefaultScreen(dpy);

    /* fprintf(stderr, "Trying to get window ID..."); */
    if (!(win = XD_get_cur_window(dpy, scrn))) {
        fprintf(stderr, "Xcell: bad window id\n");
        return(0);
    }
    /* fprintf(stderr, "Got it: 0x%lx\n", win); */

    colormode = XD_colormode(dpy, win, scrn);

    XGetWindowAttributes(dpy, win, &xwa);
    cmap = xwa.colormap;

    /* fprintf(stderr, "Trying to get window..."); */
    G_get_window(&cellhead);
    /* fprintf(stderr, "Got it\n"); */

    nrows = G_window_rows();
    ncols = G_window_cols();
    /* fprintf("stderr, "nrows=%d, ncols=%d\n", nrows, ncols); */

    gc = XCreateGC(dpy, win, gc_mask, &GC_values);

    if (!XGetGeometry(dpy, win, &drawable_root, &x, &y,
            &width, &height, &border_width, &drawable_depth)) {
        fprintf(stderr, "Xcell: bad window id\n");
        return(0);
    }

    XD_get_screen_bounds(&xl, &xr, &yt, &yb, cellhead.north,
        cellhead.south, cellhead.west, cellhead.east, width, height);

    /* send an event for loading floating colors */
    if (strcmp("float", colormode) == 0) {
        float_flag = 1;
        event.type = ClientMessage;
        event.send_event = True;
        event.display = dpy;
        event.window = win;
        event.format = 8;
        event.message_type = XA_STRING;
        strncpy((char*)(event.data.b), name, 20);
        XSendEvent(dpy, win, True, PropertyChangeMask, (XEvent*)&event);
        XFlush(dpy);
    } else {    /* for fixed, make lookup table  */
        int ncats, i;

    fprintf (stderr, "Reading color table ...\n");
        if (G_read_colors(name, mapset, &colors) == -1)
            G_fatal_error("Color file not available");
        ncats = colors.max - colors.min + 1;

    fprintf (stderr, "Translating color table ...\n");
        if (!(lookup_tbl = (u_long *)calloc(ncats+1, sizeof(u_long))))
           G_fatal_error("can't alloc color table");
        lookup_tbl[0] = XD_calc_closest_colr(cmap, colors.r0 *colors.r0,
                colors.g0 * colors.g0, colors.b0 * colors.b0);
        for (i=0; i < ncats; i++)
            lookup_tbl[i+1] = XD_calc_closest_colr(cmap,
                    colors.red[i] * colors.red[i],
                    colors.grn[i] * colors.grn[i],
                    colors.blu[i] * colors.blu[i]);
    }
    delta_x = (xr - xl) / ncols;
    delta_y = (yb - yt) / nrows;
    col_inc = 1.0 / delta_x;
    if (col_inc < 1.0)
        col_inc = 1.0;

    row_inc = 1.0 / delta_y;
    if (row_inc < 1.0)
        row_inc = 1.0;
    rect_y = (int)yt;

    fprintf (stderr, "Plotting ...\n");
    crow = 0;
    /* loop over rows */
    for (row = 0; row < nrows;
        row = (int)(crow * row_inc + 0.5)) {
        crow++;
        if (G_get_map_row(fd, cell, row) < 0)
            exit();

        hgt = (int)(yt + crow * delta_y + 0.5) - rect_y;
        hgt = (hgt < 1) ? 1 : hgt;
        iold = 0;
        ccol = 1;
        col = 0;
        rect_x = (int)xl;

        /* loop over cols */
        while (col < ncols) {
            iold = col;
            while ((next_col = (int)(ccol * col_inc + 0.5)) < ncols) {
                if (*(cell + col) != *(cell + next_col))
                    break;
                ccol++;
            }
            wid = (int)(next_col * delta_x + 0.5) -
                    (int)(iold * delta_x + 0.5);
            wid = (wid < 1) ? 1 : wid;
            if (!overlay || *(cell + col)) {
                if (float_flag)
                    XSetForeground(dpy, gc, *(cell + col) % 216 + 38);
                else
		{
		    if (cell[col])
			XSetForeground(dpy, gc,
			    lookup_tbl[cell[col] - colors.min +1]);
		    else
			XSetForeground(dpy, gc, lookup_tbl[0]);
		}
                XFillRectangle(dpy, win, gc, (int)(rect_x + 0.5),
                    (int)rect_y, (unsigned)wid, (unsigned)hgt);
            }
            rect_x += wid;
            col = next_col;
            ccol++;
        }
        rect_y += hgt;
    }
    /* change cell property */
    if (overlay) {
        if ((property = XInternAtom(dpy, "grass_overlay", True)) !=None)
            XChangeProperty(dpy, win, property, XA_STRING, 8,
                PropModeReplace, (u_char*)name, strlen(name));
    } else {
        if ((property = XInternAtom(dpy, "grass_cell", True)) != None)
            XChangeProperty(dpy, win, property, XA_STRING, 8,
                PropModeReplace, (u_char*)name, strlen(name));
    }
    XFlush(dpy);
    G_close_cell(fd);
}
