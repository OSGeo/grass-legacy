#include <stdio.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

/*
 * A polygon is drawn using the current color.  It has "number" verticies
 * which are found in the absolute coordinate pairs represented in the
 * "xarray" and "yarray" arrays.
 */

extern Display *dpy;
extern Window grwin;
extern Pixmap bkupmap;
extern GC     gc;

Polygon_abs(xarray, yarray, number)
	int *xarray, *yarray ;
	int number ;
{
    static XPoint *xyarr;
    static int num_alloc = 0 ;
    XPoint *xytmp;
    char *calloc();
    char *realloc();
    int i;

    /* The two separate x and y coord arrays must be combined for X 
     * First allocate space for the XPoint struct
     */
     
	if (num_alloc < number) {
		if (num_alloc == 0)
			xyarr = ( XPoint *) calloc(number+1, sizeof(XPoint));
		else 
			xyarr = ( XPoint *) realloc((unsigned)xyarr, (number+1) * sizeof(XPoint));
		if (xyarr == NULL) {
		fprintf(stderr,"Not enough room for XPoints\n");
		return(-1);
		}
		num_alloc = number ;
	}

    /* now move coordinate pairs together */
    xytmp = xyarr;
    for (i=0; i<number ; i++){
	xytmp->x = (short) *xarray++;
	xytmp->y = (short) *yarray++;
	xytmp++;
    }
    XFillPolygon(dpy,grwin,gc,xyarr,number,Complex,CoordModeOrigin);
    XFillPolygon(dpy,bkupmap,gc,xyarr,number,Complex,CoordModeOrigin);
}
