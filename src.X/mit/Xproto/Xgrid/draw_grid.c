/*  %W%  %G%  */

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include "gis.h"

Display *dpy;
Window win;
GC  gc;
XGCValues gc_values;
unsigned long gc_valuemask;
int scrn;
XWindowAttributes winattr;

draw_grid(grid_size, color)
int grid_size;
char *color;
{
    extern struct Cell_head window;
    double D_south, D_west;
    double D_north, D_east;
    double U_to_D_xconv, U_to_D_yconv;
    double ew_size, ns_size;
    double U_start;
    double U_x, U_y;
    int D_x, D_y;
    unsigned int window_width, window_height, border_width, depth;
    unsigned long foregr;
    int x, y;
    Window root_return;


    /* Set the display to be the default display */
    if ((dpy = XOpenDisplay("")) == NULL) {
        printf(" can't open display\n");
        return (-1);
    }
    scrn = DefaultScreen(dpy);

    win = XD_get_cur_window(dpy,
        scrn);

    XGetWindowAttributes(dpy, win,
        &winattr);



    gc = XCreateGC(dpy, win,
        0, NULL);


    XGetGeometry(dpy, win,
        &root_return, &x, &y, &window_width,
        &window_height, &border_width, &depth);



    foregr = XD_make_colr(dpy,
        win, scrn,
        winattr.colormap, color);


    XSetForeground(dpy, gc,
        foregr);

    XD_get_screen_bounds(&D_west, &D_east,
        &D_north, &D_south, window.north,
        window.south, window.west, window.east,
        window_width, window_height);


    ew_size = window.east - window.west;
    ns_size = window.north - window.south;

    U_to_D_xconv = (D_east - D_west) / ew_size;
    U_to_D_yconv = (D_south - D_north) / ns_size;

/* Draw vertical grids */
    U_start = window.east - ((int) window.east % grid_size);


    for (U_x = U_start; U_x >= window.west; U_x -= grid_size) {
        D_x = (int) ((U_x - window.west) * U_to_D_xconv + D_west);

        XDrawLine(dpy, win, gc,
            D_x, (int) D_north, D_x, (int) D_south);
    }


/* Draw horizontal grids  */
    U_start = window.north - ((int) window.north % grid_size);
    for (U_y = U_start; U_y >= window.south; U_y -= grid_size) {
        D_y = (int) ((window.north - U_y) * U_to_D_yconv +
            D_north);

        XDrawLine(dpy, win, gc,
            (int) D_west, D_y, (int) D_east, D_y);
    }

    XFlush(dpy);
}
