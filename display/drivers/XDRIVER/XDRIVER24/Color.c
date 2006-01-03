/* Set the GC foreground value to the number passed to color. All
 * subsequent graphics calls will use this number, hence they will be
 * drawn in that color's number.
 * 
 * Called by: Color() in ../lib/Color.c */

#include <stdio.h>
#include "includes.h"
#include "colors.h"
#include "gis.h"
#include "XDRIVER.h"

int color(int number)
{
    if (number >= NCOLORS || number < 0)
    {
        G_warning("Color: can't set color %d\n", number);
        return 0;
    }

    if (get_table_type() == FLOAT)
        XSetForeground(dpy, gc, number);
    else if (use_visual->class >= TrueColor)
	XSetForeground(dpy, gc, number);
    else
	XSetForeground(dpy, gc, xpixels[number]);

    return 0;
}

/*** end Color.c ***/
