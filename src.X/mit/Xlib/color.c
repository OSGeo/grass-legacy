#include <stdio.h>
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>

#include "colors.h"

u_long XD_make_colr(dpy, win, scrn, colormap, color_str)
Display *dpy;
Window win;
int scrn;
Colormap colormap;
char *color_str;
{
    Atom property, actual_type;
    u_long nitems, remaining;
    u_char *one;
    int actual_format;
    XColor exact, almost;
    u_long to_return = 0L, XD_calc_closest_colr();
    char ret_str[32];

    if ((property = XInternAtom(dpy, "grass_colormode", True)) == None){
		fprintf(stderr, "atom \"grass_colormode\" doesn't exist.\n");
		return((u_long)0L);
	}
    XGetWindowProperty(dpy, win, property, 0L, 320L, False, XA_STRING,
			&actual_type, &actual_format, &nitems, &remaining, &one);

    if (sscanf(one, "%s", ret_str) != 1)
		return((u_long)0L);
    if (strcmp(ret_str, "fixed") == 0) {
        if (!XLookupColor(dpy, colormap, color_str, &exact, &almost)) {
            fprintf(stderr, "requested color not in database.\n");
            return(-1);
        }
		to_return = XD_calc_closest_colr(colormap, almost.red,
				almost.green, almost.blue);
    } else {
        if (!strcmp(color_str, "red"))
            to_return = RED + UNTOUCHED;
        else if (!strcmp(color_str, "orange"))
            to_return = ORANGE + UNTOUCHED;
        else if (!strcmp(color_str, "yellow"))
            to_return = YELLOW + UNTOUCHED;
        else if (!strcmp(color_str, "green"))
            to_return = GREEN + UNTOUCHED;
        else if (!strcmp(color_str, "blue"))
            to_return = BLUE + UNTOUCHED;
        else if (!strcmp(color_str, "violet"))
            to_return = VIOLET + UNTOUCHED;
        else if (!strcmp(color_str, "white"))
            to_return = WHITE + UNTOUCHED;
        else if (!strcmp(color_str, "black"))
            to_return = BLACK + UNTOUCHED;
        else if (!strcmp(color_str, "gray"))
            to_return = GRAY + UNTOUCHED;
        else if (!strcmp(color_str, "brown"))
            to_return = BROWN + UNTOUCHED;
        else if (!strcmp(color_str, "magenta"))
            to_return = MAGENTA + UNTOUCHED;
        else if (!strcmp(color_str, "cyan"))
            to_return = CYAN + UNTOUCHED;
        else if (!strcmp(color_str, "gold"))
            to_return = GOLD + UNTOUCHED;
        else if (!strcmp(color_str, "khaki"))
            to_return = KHAKI + UNTOUCHED;
        else if (!strcmp(color_str, "turquoise"))
            to_return = TURQUOISE + UNTOUCHED;
        else if (!strcmp(color_str, "pink"))
            to_return = PINK + UNTOUCHED;
        else
            fprintf(stderr, "Color: %s no allowable in float mode\n",
					color_str);
    }
    return(to_return);
}

char *XD_colormode(dpy, win, scrn)
Display *dpy;
Window win;
int scrn;
{
    Atom property, actual_type;
    u_long nitems, remaining;
    int actual_format;
    u_char *one;
    char ret_str[32];
	
    if ((property = XInternAtom(dpy, "grass_colormode", True)) == None){
		fprintf(stderr, "atom \"grass_colormode\" doesn't exist.\n");
		return((u_long)0L);
	}
    XGetWindowProperty(dpy, win, property, 0L, 320L, False, XA_STRING,
			&actual_type, &actual_format, &nitems, &remaining, &one);
    if (sscanf(one, "%s", ret_str) != 1)
		return(NULL);
    if (strcmp(ret_str, "fixed") == 0)
		return("fixed");
	else if (strcmp(ret_str, "float") == 0)
		return("float");
	else 
		return(NULL);
}

