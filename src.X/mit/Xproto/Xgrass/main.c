#include <stdio.h>
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>

Display *dpy;
Atom property;
char cur_wind[32], temp[32];


main(argv, argc)
int argc;
char **argv;
{
    char *display_name = NULL;
    Atom actual_type;
    unsigned long nitems, remaining;
    unsigned char *data;
    int actual_format;

    /* connection to X server */
    if (!(dpy = XOpenDisplay(display_name))) {
        fprintf(stderr, "xgrass : cannot connect to X server %s\n",
            XDisplayName(display_name));
        exit(-1);
    }
    /* getting screen size from display structure macro */

    /* set properties */
    strcpy(cur_wind, "Grass3.1a");

    property = XInternAtom(dpy, "GRASS_WINDOW", False);
    /* fprintf(stderr, "\nid = %u\n", property); */

    XChangeProperty(dpy, DefaultRootWindow(dpy), property, XA_STRING,
			8, PropModeReplace, (u_char*)cur_wind, strlen(cur_wind));

    if (XGetWindowProperty(dpy, DefaultRootWindow(dpy), property, 0L,
        	1000L, False, XA_STRING, &actual_type, &actual_format,
			&nitems, &remaining, &data) != Success) {
		fprintf(stderr, "can't get window property");
		exit(-1);
	}
	if ((actual_type != XA_STRING) || (actual_format != 8))
    	fprintf(stderr, "Wrong atom type\n");
    if (sscanf(data, "%s", temp))
    	fprintf(stderr, "cur_window(str) = \"%s\"\n", temp);

} /*** end xgrass.c ***/
