/* -*- c-basic-offset: 4; -*-
 * Set the GC foreground value to the number passed to color. All
 * subsequent graphics calls will use this number, hence they will be
 * drawn in that color's number.
 * 
 * Called by: Color() in ../lib/Color.c */
#include <stdio.h>
#include "includes.h"
#include "../lib/colors.h"
#include "layers.h"

extern int NCOLORS;
extern Display *dpy;
extern GC gc;
extern u_long *xpixels;
extern int table_type;

int SetXColor (int number)
{
	layer_t* l = get_scratch_layer();

    if ((number >= NCOLORS) || (number < 0)) {
        fprintf(stderr, "Color: can't set color %d\n", number);
        return 0;
    }

	set_current_color(number);

    if (table_type == FIXED) {
        XSetForeground(dpy, gc, xpixels[number]);
		if (l!=NULL)
			XSetForeground(dpy, l->gc, xpixels[number]);
	}
    else {
        XSetForeground(dpy, gc, (u_long) number);
		if (l!=NULL)
			XSetForeground(dpy, l->gc, (u_long) number);
	}
/*  	fprintf(stderr, "Color %d\n", number); */
 
    return 0;
}

/*** end Color.c ***/
