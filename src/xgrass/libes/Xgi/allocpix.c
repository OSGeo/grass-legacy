static char rcsid[] = "@(#)XGRASS $Id: allocpix.c,v 0.0 1992/05/05 14:56:09 sink Exp sink $";
/*
 * File: allocpix.c
 *
 * Desc: This file contains functions for allocating a private color cell
 *       out of a colormap, based on the red, green, blue components described
 *       in the XColor structure passed in. It will fill XColor structure, and
 *       return non-zero on success. It will return zero on any failure.
 *
 * Auth: Kurt Buehler
 *
 * Date: Fri Jan  3 08:35:55 CST 1992
 *
 * Modification History:
 *
 *
 */
#include "xgrass_lib.h"

#ifdef _NO_PROTO
XgAllocatePixel(display,cmap,xcolor)
Display *display;
Colormap cmap;
XColor *xcolor;
#else 
XgAllocatePixel(
	       Display *display,
	       Colormap cmap,
	       XColor *xcolor)
#endif
{
    unsigned long pixel;
    int result;

    result = XAllocColorCells( display, cmap, False, NULL, 0, &pixel, 1);
    if ( result == 0 ) return result;
    xcolor->flags = DoRed | DoGreen | DoBlue;
    xcolor->pixel = pixel;
    XStoreColor( display, cmap, xcolor); /* EWS removed ,1 */
    return result;
}


#ifdef _NO_PROTO
XgAllocateNamedPixel(display,cmap,xcolor,name)
Display *display;
Colormap cmap;
XColor *xcolor;
char *name;
#else 
XgAllocateNamedPixel(
	       Display *display,
	       Colormap cmap,
	       XColor *xcolor,
	       char *name)
#endif
{
    unsigned long pixel;
    XColor exact;
    int result;

    result = XAllocColorCells( display, cmap, False, NULL, 0, &pixel, 1);
    if ( result == 0 ) return result;
    result = XParseColor(display,cmap,name,xcolor);
    if ( result == 0 ) return result;
    xcolor->pixel = pixel;
    XStoreColor( display, cmap, xcolor); /* EWS removed ,1 */
    return result;
}

