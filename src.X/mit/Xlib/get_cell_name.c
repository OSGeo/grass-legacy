#include <stdio.h>
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>

char *XD_get_cell_name(dpy, win, ret_str)
Display *dpy;
Window win;
char *ret_str;
{
    Atom property, actual_type;
    unsigned long nitems, remaining;
    unsigned char *one;
    int actual_format;

    if ((property = XInternAtom(dpy, "grass_cell", True)) == None) {
		fprintf(stderr,
			"XD_get_cell_name: no \"grass_cell\" atom exists.\n");
		return(NULL);
	}
    XGetWindowProperty(dpy, win, property, 0L, 320L, False, XA_STRING,
		&actual_type, &actual_format, &nitems, &remaining, &one);

    if (sscanf(one, "%s", ret_str) != 1)
		return(NULL);
    /* printf("\n property(str) = %s", ret_str); */
    return (ret_str);
}

