/* @(#)main.c   2.2   7/24/87 */

/*
 *   Xvect
 *
 *   Usage:  Xvect mapname [color]
 *           Xvect name=mapname color=name
 *
 *   Draw the binary vector (dig) file that
 *   the user wants displayed on top of the current image.
 */

#include <stdio.h>
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include "gis.h"
#include "digit.h"
#define MAIN
#include "options.h"

#define USAGE   "name=mapname [color=name]"

Display *dpy;
Window win;
int scrn;
GC  gc;
XGCValues gc_values;
u_long gc_value_mask;
Colormap colormap;

struct Cell_head window;

main(argc, argv)
int argc;
char **argv;
{
    FILE *fp;
    char *mapset, buff[128];
    u_long XD_make_colr();
    extern int stash_away();
    XWindowAttributes xwa;
    Atom property;

	/* Initialize the GIS calls */
    G_gisinit("display vector map");
	/* Check command line */
    set_default_options();

    if (D_parse_command(argc, argv, variables,
            n_variables, stash_away) || *map_name == 0) {
        fprintf(stderr, "Usage: %s %s\n", argv[0], USAGE);
        exit(-1);
    }
	/* Make sure map is available */
    mapset = G_find_file("dig", map_name, "");
    if (mapset == NULL) {
        sprintf(buff, "Vector file [%s] not available", map_name);
        G_fatal_error(buff);
    }
	/* Check for and read dig header info */

    if (!(fp = G_fopen_old("dig", map_name, mapset))) {
        sprintf(buff, "Can't open vector file [%s] in [%s]",
            map_name, mapset);
        G_fatal_error(buff);
    }
    if (dig_init(fp) == -1) {
        (void)fclose(fp);
        G_fatal_error("Problem in initial read of digit file");
    }
    /* Set the display to be the default display */
    if (!(dpy = XOpenDisplay(NULL))) {
        fprintf(stderr, "in %s: can't open display\n", argv[0]);
        return (-1);
    }
    scrn = DefaultScreen(dpy);
    win = XD_get_cur_window(dpy, scrn);

    G_get_window(&window);

    XGetWindowAttributes(dpy, win, &xwa);
    colormap = xwa.colormap;

    /* Decipher color */
    if (argc == 3)
        gc_values.foreground = XD_make_colr(dpy, win, scrn, colormap,
				color);

    gc = XCreateGC(dpy, win, GCForeground, &gc_values);
    dig_print_header();

	/* Do the plotting */
    if (dig_plot_all_lines(fp) == -1) {
        fclose(fp);
        G_fatal_error("Problem in reading of vector file");
    }
    (void)fclose(fp);

    if ((property = XInternAtom(dpy, "grass_vect", True)) != None)
    	XChangeProperty(dpy, win, property, XA_STRING, 8,
				PropModeReplace, (u_char*)map_name, strlen(map_name));
    XFlush(dpy);
    XFreeGC(dpy, gc);
}

debugf()
{
    /* null function while 'digit' is being debugged  */
}
