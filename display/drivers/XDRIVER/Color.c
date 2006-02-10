/* Set the GC foreground value to the number passed to color. All
 * subsequent graphics calls will use this number, hence they will be
 * drawn in that color's number.
 * 
 * Called by: Color() in ../lib/Color.c */

#include <stdio.h>
#include "includes.h"
#include <grass/colors.h>
#include <grass/gis.h>
#include "XDRIVER.h"

static int currentColor;

void XD_color(int number)
{
	if (number >= NCOLORS || number < 0)
	{
		G_warning("Color: can't set color %d\n", number);
		return;
	}

	currentColor = number;

	if (DRV_get_table_type() == FLOAT)
		XSetForeground(dpy, gc, number);
	else if (use_visual->class >= TrueColor)
		XSetForeground(dpy, gc, number);
	else
		XSetForeground(dpy, gc, xpixels[number]);
}

int XD_get_color(void)
{
	return currentColor;
}

