#include <stdio.h>
#include "defines.h"
/*
 * Set the GC foreground value to the number passed to color. All
 * subsequent graphics calls will use this number, hence they will be 
 * drawn in that color's number.
 *
 * Called by:
 *      Color() in ../lib/Color.c
 */
#include <X11/Xlib.h>
#include <X11/Xutil.h>

extern int NCOLORS ;
extern Display *dpy;
extern GC gc;

color(number)
	int number ;
{
	if (number >= NCOLORS)
		return ;
	XSetForeground(dpy,gc, (unsigned long) (number+COLOR_OFFSET));
}
