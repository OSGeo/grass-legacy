
#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xos.h>
#include "options.h"
#include "gis.h"

Display *dpy;
Window win;
int scrn;
XEvent report;

where_am_i()
{
    char buffer[200];
    char lonbuf[50], latbuf[50];
    char temp[100];
    double lat, lon;
    int screen_x, screen_y;
    double east, north;
    extern struct Cell_head window;
    double xl, xr, yt, yb;
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

    XSelectInput(dpy, win, ButtonPressMask);

    XGetGeometry(dpy, win, &root_return, &x, &y, &window_width,
        &window_height, &border_width, &depth);

    XD_get_screen_bounds(&xl, &xr, &yt, &yb, window.north, window.south,
		window.west, window.east, window_width, window_height);

    if (mode == LOUD)
        G_clear_screen();
    if (mode == LOUD) {
        fprintf(stderr, "\n\nButtons:\n");
        fprintf(stderr, "Left:   where am i\n");
        fprintf(stderr, "Middle: where am i\n");
        fprintf(stderr, "Right:  quit this\n\n\n");

        fprintf(stderr, "%-13s  %-13s", "EAST:", "NORTH:");
        if (have_spheroid)
            fprintf(stderr, "  %-20s  %-20s", "LON:", "LAT:");
        fprintf(stderr, "\n");
    }
    while (1) {
        XNextEvent(dpy, &report);

        if (report.type == ButtonPress) {
            if (((XButtonPressedEvent *) & report)->button == 3)
                exit(-1);

            screen_x = ((XButtonPressedEvent *) & report)->x;
            screen_y = ((XButtonPressedEvent *) & report)->y;

            east = window.west + (screen_x - xl)
                * (window.east - window.west) / (xr - xl);

            north = window.north - (screen_y - yt)
                * (window.north - window.south) / (yb - yt);
            if (mode == SILENT) {
                printf("%13.2f  %13.2f %d", east, north,
                    ((XButtonPressedEvent *) & report)->button);
                return (0);
            }
            sprintf(buffer, "%13.2f  %13.2f", east, north);
            if (have_spheroid) {
                CC_u2ll_north(north);
                CC_u2ll(east, &lat, &lon);
                CC_lon_format(lon, lonbuf);
                CC_lat_format(lat, latbuf);
                sprintf(temp, "  %20s  %20s", lonbuf, latbuf);
                strcat(buffer, temp);
            }
            show(buffer);
        }   /* end of if over button press */
    }   /* end of while */
}

static show(buf)
char *buf;
{
    char *b;

    if (!isatty(1))
        printf("%s\n", buf);
    for (b = buf; *b; b++)
        fprintf(stderr, "%c", *b);
    for (b = buf; *b; b++)
        fprintf(stderr, "\b");
}
