#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <stdio.h>

/*
 * The systems color represented by "number" is set using the color component
 * intensities found in the "red", "grn", and "blu" variables.  A value of
 * 0 represents 0 intensity; a value of 255 represents 100% intensity.
 *
 * This routine uses several externals :
 *  dpy - the pointer to the X display defined in Graph_Set.c,
 *  NCOLORS - the maximum no. of colors possible in the colormap,
 *  grasscmap - the colormap for the grass window created in Graph_Set.c
 */

extern int NCOLORS ;
extern Display *dpy ;
extern Colormap grasscmap;

reset_color(number, red, grn, blu)
	int number ;
	int red, grn, blu;
{
	XColor sd;
	int r,g,b ;

	if (number >= NCOLORS) return;	/* ignor out-of-range vals */

	sd.flags = ( DoRed | DoGreen | DoBlue );

	/* convert to the 0-65535 range for X,
	 * put into XColor struct, and set.
	 */
	
	sd.pixel = number;
	sd.red = red*257;
	sd.green = grn*257;
	sd.blue = blu*257;
	
	XStoreColor(dpy, grasscmap, &sd);
}
