/* Set the GC foreground value to the number passed to color. All
 * subsequent graphics calls will use this number, hence they will be
 * drawn in that color's number.
 * 
 * Called by: Color() in ../lib/Color.c */
#include <stdio.h>
#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "../lib/colors.h"

extern int NCOLORS;
extern Display *dpy;
extern GC gc;
extern u_long xpixels[];
extern int table_type;

SetXColor(number)
int number;
{
    if ((number >= NCOLORS) || (number < 0)) {
        fprintf(stderr, "Color: can't set color %d\n", number);
        return;
    }
    if (table_type == FIXED)
        XSetForeground(dpy, gc, xpixels[number]);
    else
        XSetForeground(dpy, gc, (u_long) number);
}

/*** end Color.c ***/
