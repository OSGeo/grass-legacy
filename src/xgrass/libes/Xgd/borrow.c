#include "xgrass_dlib.h"

#define ABS(x) (((x) < 0) ? -(x):(x))

Boolean
#ifdef _NO_PROTO
_XgdDetermineIfClose(color, entry)
    XColor *color;
    int *entry;
#else
_XgdDetermineIfClose( XColor *color, int *entry)
#endif
{
    double x, closestX = HUGE; 
    double dr, dg, db;
    int closest;
    unsigned short inred, ingreen, inblue;
    unsigned short red, green, blue;
    int i;
    
    inred = color->red >> 8;
    ingreen = color->green >> 8;
    inblue = color->blue >> 8;
    for ( i = 0; i < __XGDTotalCells; i++ ) {
        /* disallow the highlight color for borrowing */
        if ( __XGDLut[i].pixel != __XGDHighlight ) {
	    red = __XGDLut[i].red >> 8;
	    green = __XGDLut[i].green >> 8;
	    blue = __XGDLut[i].blue >> 8;
	    dr = (double)red - (double)inred;
	    dg = (double)green - (double)ingreen;
	    db = (double)blue - (double)inblue;
	    x = (dr * dr) + (dg * dg) + (db * db); 
	    if ( x < closestX ){
		closest = i;
		closestX = x;
	    }
        }
    }
    *entry = closest;
    if ( closest != -1 ) {
        return True;
    }
    return False;
}

#ifdef _NO_PROTO
_XgdBorrowColor(color)
    XColor *color;
#else
_XgdBorrowColor( XColor *color)
#endif
{
    int entry;

    if ( _XgdDetermineIfClose(color, &entry) ) {
	color->pixel = __XGDLut[entry].pixel;
	if ( __XGDLut[entry].mode == XG_LUT_MODE_RW ) {
	    if ( __XGDLut[entry].status == XG_LUT_STATUS_IN_USE ) {
		__XGDLut[entry].status = XG_LUT_STATUS_SHARED;
	    }
	    __XGDLut[entry].accesses++;
	}
	return;
    }
}

