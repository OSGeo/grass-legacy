/* @(#)threed.c 2.1   10/1/87 */

#include <stdio.h>
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "gis.h"
#include "3d.h"
#include "options.h"

static CELL *cell;
static CELL *elv_array_1;
static CELL *elv_array_2;
static int fd;
static int elev;

threed(interactive)
{
    int col_beg = 0;    /* western edge denoting window (array) */
    int col_end = 0;    /* eastern edge denoting window (array) */
    int row_beg = 0;    /* southern edge denoting window (array) */
    int row_end = 0;    /* northern edge denoting window (array) */
    char buffer[128];
    int row_dir = 0;
    int col_dir = 0;
    int break_row;

	/* Open the necessary cell files */
    if ((fd = G_open_cell_old(file, file_mapset)) == -1) {
        sprintf(buffer, "Cell file [%s in %s] not available",
            file, file_mapset);
        G_fatal_error(buffer);
    }
    if ((elev = G_open_cell_old(elevfile, elevfile_mapset)) == -1)
        G_fatal_error(buffer);

    /* Allocate space for arrays */
    cell = G_allocate_cell_buf();
    elv_array_1 = G_allocate_cell_buf();
    elv_array_2 = G_allocate_cell_buf();

    /* Set up for drawing map */
    switch (direction) {
    case NORTH_WEST:
    case WEST_NORTH:
        row_beg = window.rows - 1;
        row_end = 1;
        row_dir = -1;
        col_beg = window.cols - 1;
        col_end = 1;
        col_dir = -1;
        printf("View from North West\n");
        break;

    case SOUTH_WEST:
    case WEST_SOUTH:
        row_beg = 1;
        row_end = window.rows - 1;
        row_dir = 1;
        col_beg = window.cols - 1;
        col_end = 1;
        col_dir = -1;
        printf("View from South West\n");
        break;

    case NORTH_EAST:
    case EAST_NORTH:
        row_beg = window.rows - 1;
        row_end = 1;
        row_dir = -1;
        col_beg = 1;
        col_end = window.cols - 1;
        col_dir = 1;
        printf("View from North East\n");
        break;

    case SOUTH_EAST:
    case EAST_SOUTH:
        row_beg = 1;
        row_end = window.rows - 1;
        row_dir = 1;
        col_beg = 1;
        col_end = window.cols - 1;
        col_dir = 1;
        printf("View from South East\n");
        break;
    }

    if (interactive == 1) {
        setbuf(stdout, 0);
        printf("\nRows %d to %d at:    ", row_beg, row_end);
    }
    /* Provide for plotting from north and south if necessary */
    if ((from_northing > window.south) && (from_northing < window.north)) {
        break_row = (int) ((window.north - from_northing) /
				window.ns_res + .5);
        do_plot(row_beg, break_row + row_dir, row_dir,
				col_beg, col_end, col_dir, interactive);
        do_plot(row_end, break_row - row_dir, -row_dir,
				col_beg, col_end, col_dir, interactive);
    } else {
        do_plot(row_beg, row_end, row_dir, col_beg, col_end,
				col_dir, interactive);
    }
    free(cell);
    free(elv_array_1);
    free(elv_array_2);
    G_close_cell(fd);
    G_close_cell(elev);
}


do_plot(row_beg, row_end, row_dir, col_beg, col_end, col_dir, interactive)
{
    int cur_colr;
    XPoint poly[4];
	u_long *lookup_tbl, XD_calc_closest_colr();
    extern Display *dpy;
    extern Window win;
    extern GC gc;
	int scrn = DefaultScreen(dpy);
	char *XD_colormode();
	int float_flag = 0;
    int col, row, ncats, i;
	struct Colors colors;
	Colormap cmap;
	XWindowAttributes xwa;
	/* Jim - these should be allocated, based on the window */
    int scr_x1[1280], scr_y1[1280], scr_x2[1280], scr_y2[1280];
    int *scr_x_1 = NULL, *scr_x_2 = NULL;
	int *scr_y_1 = NULL, *scr_y_2 = NULL;
    char flip;

    set_signals();

	/* Set up colors correctly P. Thompson */
	if (G_read_colors(file, file_mapset, &colors) == -1)
	   G_fatal_error("Color file not available");
	ncats = colors.max - colors.min + 1;
	XGetWindowAttributes(dpy, win, &xwa);
	cmap = xwa.colormap;
	if (!(lookup_tbl = (u_long *)calloc(ncats, sizeof(u_long))))
	   G_fatal_error("can't alloc color table");
	for (i=0; i < ncats; i++)
		lookup_tbl[i] = XD_calc_closest_colr(cmap,
				colors.red[i] * colors.red[i],
				colors.grn[i] * colors.grn[i],
				colors.blu[i] * colors.blu[i]);
	if (strcmp("float", XD_colormode(dpy, win, scrn)) == 0)
		float_flag = 1;

	/* Now draw the map */
    G_get_map_row_nomask(elev, elv_array_1, row_beg);
    Screen_calc(elv_array_1, exag, scr_x1, scr_y1, row_beg, &window);

    flip = 0;
    cur_colr = 0;
    for (row = row_beg; row != row_end; row += row_dir) {
        if (check_signal())
            break;

        if (interactive == 1)
            printf("\b\b\b\b%4d", row);
        switch (flip) {
        case 1:
            G_get_map_row_nomask(elev, elv_array_1, row + row_dir);
            Screen_calc(elv_array_1, exag, scr_x1, scr_y1, row, &window);
            scr_x_1 = scr_x2;
            scr_y_1 = scr_y2;
            scr_x_2 = scr_x1;
            scr_y_2 = scr_y1;
            flip = 0;
            break;
        case 0:
            G_get_map_row_nomask(elev, elv_array_2, row + row_dir);
            Screen_calc(elv_array_2, exag, scr_x2, scr_y2, row, &window);
            scr_x_1 = scr_x1;
            scr_y_1 = scr_y1;
            scr_x_2 = scr_x2;
            scr_y_2 = scr_y2;
            flip = 1;
            break;
        }
        if (G_get_map_row(fd, cell, row) < 0) {
			G_fatal_error("no more rows in map");
			exit();
		}
        /* R_color polygons */
        for (col = col_beg; col != col_end; col += col_dir) {
            if (cur_colr != cell[col])
				if (float_flag)
					XSetForeground(dpy, gc, *(cell + col) % 216 + 38);
				else
                	XSetForeground(dpy, gc,
							lookup_tbl[cell[col] - colors.min]);
            poly[0].x = (short)scr_x_1[col];
            poly[1].x = (short)scr_x_1[col + col_dir];
            poly[2].x = (short)scr_x_2[col + col_dir];
            poly[3].x = (short)scr_x_2[col];
            poly[0].y = (short)scr_y_1[col];
            poly[1].y = (short)scr_y_1[col + col_dir];
            poly[2].y = (short)scr_y_2[col + col_dir];
            poly[3].y = (short)scr_y_2[col];
            XFillPolygon(dpy, win, gc, poly, 4, Convex,CoordModeOrigin);
        }
    }
    XFlush(dpy);
    printf("\n");
}

