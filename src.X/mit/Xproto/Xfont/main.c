/*
 *   Xfont
 *   Usage:  Xfont font
 */

#include <stdio.h>
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include "gis.h"

#define USAGE   "font"

Display *dpy;
int scrn;
Window win;
Atom font;

main(argc, argv)
int argc;
char **argv;
{
    char buff[128];

    /* Initialize the GIS calls */
    G_gisinit(argv[0]);
    /* Set the display to be the default display */
    if (!(dpy = XOpenDisplay(NULL))) {
        fprintf(stderr, " can't open display\n");
        exit(-1);
    }
    scrn = DefaultScreen(dpy);
    win = XD_get_cur_window(dpy, scrn);

    /* Check command line */
    if (argc != 2) {
        fprintf(stderr, "Usage: %s %s\n", argv[0], USAGE);
        exit(-1);
    }
    if (strncmp("help", argv[1], 4) == 0) {
        sprintf(buff, "ls %s/fonts", G_gisbase());
        system(buff);
    } else {
        strcpy(buff, argv[1]);
        if ((font = XInternAtom(dpy, "grass_font", True)) != None)
        	XChangeProperty(dpy, win, font, XA_STRING, 8,
				PropModeReplace, (u_char*)argv[1], strlen(argv[1]));
    }
    XFlush(dpy);
}

