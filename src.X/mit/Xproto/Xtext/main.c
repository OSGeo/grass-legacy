/*
 *
 *   Usage:  Xtext [color] [size] [beg_line]
 *
 *   Draw text in a text window.   Text lines come from stdin.
 *   text control:
 *      .C color_name        change color
 *      .S size              change text size
 *      .B 0                 bold (double print) off
 *      .B 1                 bold (double print) on
 *      .F name              change font to name
 */

#include <stdio.h>
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "driver.h"
#define MAIN
#include "options.h"
#define USAGE   "[color=name] [size=num] [line=num]"

Display *dpy;
Window win;
Colormap colormap;
int scrn;
GC  gc;
XGCValues values;


main(argc, argv)
int argc;
char **argv;
{
    char buff[128];
    char *gets();
    char *cmd_ptr;
    int tsize;
    unsigned long colr;
    int cur_dot_row;
    int dots_per_line;
    int bold;
    int status;
    extern int stash_away(), init_font(), Text_size(), Move_abs(), Text();
    Window root_return;
    int x, y;
	unsigned window_width, window_height, border_width, depth;
    XWindowAttributes xwa;

    /* Set the display to be the default display */
    if (!(dpy = XOpenDisplay(NULL))) {
        fprintf(stderr, " can't open display\n");
        return (-1);
    }
    scrn = DefaultScreen(dpy);
    win = XD_get_cur_window(dpy, scrn);
    gc = XCreateGC(dpy, win, 0, None);
    XGetGeometry(dpy, win, &root_return, &x, &y, &window_width,
        &window_height, &border_width, &depth);

    XGetWindowAttributes(dpy, win, &xwa);
    colormap = xwa.colormap;

	/* Initialize the GIS calls */
    G_gisinit(argv[0]);

	/* Check command line */
    set_default_options();

    if (D_parse_command(argc, argv, variables, n_variables, stash_away)) {
        fprintf(stderr, "Usage: %s %s\n", argv[0], USAGE);
        exit(-1);
    }
	/* font */
    status = init_font("romand");
    if (status == -1)
        fprintf(stderr, "\n allo");

	/* Figure out where to put text */
    dots_per_line = (int) (size / 100.0 * (float) window_height);
    tsize = (int) (.8 * (float) dots_per_line);

    cur_dot_row = (start_line - 1) * dots_per_line;

    Text_size(tsize, tsize);

    colr = XD_make_colr(dpy,
        win, scrn,
        colormap, color);

    XSetForeground(dpy, gc, colr);

    bold = 0;

	/* Do the plotting */
    while (gets(buff)) {
        if (*buff == '.') {
            for (cmd_ptr = buff + 2; *cmd_ptr == ' '; cmd_ptr++);
            switch (buff[1] & 0x7F) {
            case 'F':   /* font */
                init_font(cmd_ptr);
                break;
            case 'C':   /* color */

                colr = XD_make_colr(dpy, win, scrn,
                    colormap, cmd_ptr);

                XSetForeground(dpy, gc, colr);
                break;
            case 'S':   /* size */
                if (sscanf(cmd_ptr, "%f", &size)) {
                    dots_per_line = (int) (size / 100.0 * (float) window_height);
                    tsize = (int) (.8 * (float) dots_per_line);
                    Text_size(tsize, tsize);
                }
                break;
            case 'B':   /* bold */
                if (!sscanf(cmd_ptr, "%d", &bold))
                    bold = 0;
                break;
            default:
                break;
            }
        } else {
            cur_dot_row += dots_per_line;

            Move_abs(5, cur_dot_row);

            if (bold) {
                values.line_width = 2;
                XChangeGC(dpy, gc,
                    GCLineWidth, &values);

            } else {
                values.line_width = 0;
                XChangeGC(dpy, gc,
                    GCLineWidth, &values);
            }

            Text(buff);
            XFlush(dpy);
        }
    }
}

