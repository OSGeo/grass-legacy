/*  %W%  %G%  */
/*
 *   Xwhat
 *   Usage:  Xwhat [layer names]
 */

#define GLOBAL
#include "what.h"

#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

Display *dpy;
Window win;
int scrn;
XEvent report;
Mask event_mask;

main(argc, argv)
int argc;
char **argv;
{
    char temp[128];
	int width, i;
    int row, col;
    int nrows, ncols;
    CELL *buf;
    struct Cell_head window;
    int screen_x, screen_y;
    double east, north;
    int button;
    double XD_get_d_north(), XD_get_d_south();
    double XD_get_d_east(), XD_get_d_west();
    double XD_d_to_u_row(), XD_d_to_u_col();
    char *XD_get_cell_name();
    Window root_return;
    int x, y;
	unsigned window_width, window_height, border_width, depth;

    /* Initialize the GIS calls */
    G_gisinit(argv[0]);
    G_get_window(&window);

    /* Set the display to be the default display */
    if (!(dpy = XOpenDisplay(NULL))) {
        fprintf(stderr, " can't open display\n");
        exit(-1);
    }
    scrn = DefaultScreen(dpy);
    win = XD_get_cur_window(dpy, scrn);

    if (!XGetGeometry(dpy, win, &root_return, &x, &y, &window_width,
        	&window_height, &border_width, &depth))
        fprintf(stderr, "can't get window geometry\n");

    XSelectInput(dpy, win, ButtonPressMask);

    if (XD_do_conversions(&window, window_width, window_height))
        G_fatal_error("Error in calculating conversions");

    nlayers = 0;
    for (i = 1; i < argc; i++) {
        if (nlayers >= MAX_LAYERS) {
            fprintf(stderr, "warning: only %d layers allowed\n",
                MAX_LAYERS);
            break;
        }
        if (strcmp(argv[i], "-") == 0) {
            XD_get_cell_name(dpy, win, temp);

            if (*temp == 0)
                fprintf(stderr, "warning: no data layer drawn in current window\n");
            else if ((fd[nlayers] = opencell(temp, name[nlayers],
                        mapset[nlayers])) >= 0)
                nlayers++;
        } else if ((fd[nlayers] = opencell(argv[i], name[nlayers],
                    mapset[nlayers])) >= 0)
            nlayers++;
    }

    if (argc == 1) {
        XD_get_cell_name(dpy, win, temp);

        if (*temp == 0)
            fprintf(stderr, "warning: no data layer drawn in current window\n");
        else if ((fd[nlayers] = opencell(temp, name[nlayers]
                    ,mapset[nlayers])) >= 0)
            nlayers++;
    }
    if (nlayers == 0)
        exit(0);

    for (i = 0; i < nlayers; i++) {
        if (G_read_cats(name[i], mapset[i], &cats[i]) < 0)
            cats[i].num = -1;
    }

    nrows = window.rows;
    ncols = window.cols;
    buf = G_allocate_cell_buf();
    width = 0;
    for (i = 0; i < nlayers; i++) {
        int n;

        n = strlen(name[i]);
        if (n > width)
            width = n;
    }

    while (1) {
        show_mouse();

        XNextEvent(dpy, &report);

        if (report.type == ButtonPress) {
            screen_x = ((XButtonPressedEvent *) & report)->x;
            screen_y = ((XButtonPressedEvent *) & report)->y;
            button = ((XButtonPressedEvent *) & report)->button;

            if (button == 2)
                continue;
            if (button == 3)
                break;

            east = XD_d_to_u_col((double) screen_x);
            north = XD_d_to_u_row((double) screen_y);

            row = (window.north - north) / window.ns_res;
            col = (east - window.west) / window.ew_res;


            if (row < 0 || row >= nrows)
                continue;
            if (col < 0 || col >= ncols)
                continue;

            show_utm(north, east);

            for (i = 0; i < nlayers; i++)
                if (G_get_map_row(fd[i], buf, row) < 0)
                    show_cat(width, name[i], 0, "error reading cell file");
                else
                    show_cat(width, name[i], buf[col], G_get_cat(buf[col], &cats[i]));
        }
    }   /* end of while */
}

