/* @(#)p_lines.c    2.4   9/21/87 */

#include "digit.h"
#include "dig_conv.h"
#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "gis.h"

extern struct Cell_head window;

dig_plot_all_lines(fp)
FILE *fp;
{
    int status;
    Window root_return;
    int x, y;
    unsigned int width, height, border_width, depth;
    XRectangle clip_rect[1];
    extern Display *dpy;
    extern Window win;
    extern GC gc;
    struct line_pnts line_p;

    double D_get_u_west();
    double D_get_u_east();
    double D_get_u_north();
    double D_get_u_south();
    double D_get_d_west();
    double D_get_d_east();
    double D_get_d_north();
    double D_get_d_south();

    double D_get_u_to_d_xconv();
    double D_get_u_to_d_yconv();

    dig_U_west = window.west;
    dig_U_east = window.east;
    dig_U_north = window.north;
    dig_U_south = window.south;

    /* set the clipping          */
    XGetGeometry(dpy, win, &root_return,
        &x, &y, &width, &height,
        &border_width, &depth);

    XD_get_screen_bounds(&xl, &xr, &yt, &yb,
        dig_U_north, dig_U_south, dig_U_west, dig_U_east,
        width, height);

    dig_U_to_D_xconv = (xr - xl) /
        (window.east - window.west);
    dig_U_to_D_yconv = (yb - yt) /
        (window.north - window.south);


    clip_rect[0].x = (short) xl;
    clip_rect[0].y = (short) yt;
    clip_rect[0].width = (unsigned short) (xr - xl);
    clip_rect[0].height = (unsigned short) (yb - yt);

    XSetClipRectangles(dpy, gc, 0, 0,
        clip_rect, 1, Unsorted);




    dig_init_box(dig_U_north, dig_U_south, dig_U_east, dig_U_west);
    line_p.alloc_points = 0;

    while (1) {
        status = dig_read_line_struct_in_box(fp, &line_p);
        if (status == -1) {
            return (status);
        }
        if (status == -2)
            return (0);

        dig_plot_all_coors(&line_p);
    }
}
