/* @(#)colormode.c  2.1   6/26/87 */

/*
 *   Xcolormode
 *   Usage:  Xcolormode mode=fixed/float
 *           Xcolormode offset=num
 */

#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include "gis.h"
#define MAIN
#include "options.h"

#define USAGE1  "fixed/float"

Display *dpy;
int scrn;
Window win;

main(argc, argv)
int argc;
char **argv;
{
    extern int stash_away();
	XClientMessageEvent event;
	char color_mode[20];
	Atom colormode;

	/* Check command line */
    set_default_options();

    if (D_parse_command(argc, argv, variables, n_variables, stash_away)) {
        fprintf(stderr, "Usage: %s %s\n", argv[0], USAGE1);
        exit(-1);
    }
    /* Set the display to be the default display */
    if (!(dpy = XOpenDisplay(NULL))) {
        fprintf(stderr, " can't open display\n");
        exit (-1);
    }
    scrn = DefaultScreen(dpy);
    win = XD_get_cur_window(dpy, scrn);

    if ((colormode = XInternAtom(dpy, "grass_colormode", True))== None){
		fprintf(stderr, "No \"grass_colormode\" atom found\n");
		exit(-1);
	}
    event.type = ClientMessage;
    event.send_event = True;
    event.display = dpy;
    event.window = win;
    event.format = 8;
    event.message_type = XA_STRING;
    if (mode == FLOAT) {
        strcpy(color_mode, "float");
        strcpy((char *)(event.data.b), "float");
    } else {
        strcpy(color_mode, "fixed");
        strcpy((char *)(event.data.b), "fixed");
    }

    if (!XSendEvent(dpy, win, True, PropertyChangeMask,(XEvent*)&event))
		fprintf(stderr, "Event not sent\n");

    XChangeProperty(dpy, win, colormode, XA_STRING, 8, PropModeReplace,
        (u_char*)color_mode, strlen(color_mode));
    XFlush(dpy);
	exit(0);
}

