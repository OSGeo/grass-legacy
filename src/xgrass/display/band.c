#include "xgdisp.h"
#define ABS(x) ((x) < 0 ? -(x):(x))
#define ABS(x) ((x) < 0 ? -(x):(x))

void
#ifdef _NO_PROTO
BandLine(x, y, mode)
int x; 
int y;
int mode;
#else
BandLine(int x, int y, int mode)
#endif
{
    static int startx, starty;
    static int lastx, lasty;

    switch(mode) {
    case XGD_BAND_INIT:
	startx = x;
        starty = y;
        lastx = x;
        lasty = y;
        break;
    case XGD_BAND:
	XDrawLine(Global.display, XtWindow(Global.drawArea), Global.xorGC,
	    startx, starty, lastx, lasty);
        lastx = x;
        lasty = y;
        break;
    case XGD_BAND_END:
	XDrawLine(Global.display, XtWindow(Global.drawArea), Global.xorGC,
	    startx, starty, lastx, lasty);
        break;
    }
    if ( mode != XGD_BAND_END )
	XDrawLine(Global.display, XtWindow(Global.drawArea), Global.xorGC,
	    startx, starty, x, y);
}

void
#ifdef _NO_PROTO
BandRect(x, y, mode, equal)
int *x; 
int *y;
int mode;
Boolean equal;
#else
BandRect(int *x, int *y, int mode, Boolean equal)
#endif
{
    static int startx, starty;
    static int lastx, lasty;

    switch(mode) {
    case XGD_BAND_INIT:
        startx = *x;
        starty = *y;
        lastx = *x;
        lasty = *y;
        break;
    case XGD_BAND:
        if ( equal ) {
            int width, height;
            int gap;

            width = ABS(startx - *x);
            height = ABS(starty - *y);
            gap = ABS(width - height);

            if ( width > height ) {
                if ( *y < starty )
		    *y = *y - gap;
                else
		    *y = *y + gap;
            } else {
                if ( *x < startx )
		    *x = *x - gap;
                else
		    *x = *x + gap;
            }
        }
	XDrawLine(Global.display, XtWindow(Global.drawArea), Global.xorGC,
	    startx, starty, startx, lasty);
	XDrawLine(Global.display, XtWindow(Global.drawArea), Global.xorGC,
	    startx, lasty, lastx, lasty);
	XDrawLine(Global.display, XtWindow(Global.drawArea), Global.xorGC,
	    lastx, lasty, lastx, starty);
	XDrawLine(Global.display, XtWindow(Global.drawArea), Global.xorGC,
	    lastx, starty, startx, starty);
        lastx = *x;
        lasty = *y;
        break;
    case XGD_BAND_END:
	if ( equal ) {
	    int width, height;
	    int gap;

	    width = ABS(startx - *x);
	    height = ABS(starty - *y);
	    gap = ABS(width - height);

	    if ( width > height ) {
		if ( *y < starty )
		    *y = *y - gap;
		else
		    *y = *y + gap;
	    } else {
		if ( *x < startx )
		    *x = *x - gap;
		else
		    *x = *x + gap;
	    }
	}
	XDrawLine(Global.display, XtWindow(Global.drawArea), Global.xorGC,
	    startx, starty, startx, lasty);
	XDrawLine(Global.display, XtWindow(Global.drawArea), Global.xorGC,
	    startx, lasty, lastx, lasty);
	XDrawLine(Global.display, XtWindow(Global.drawArea), Global.xorGC,
	    lastx, lasty, lastx, starty);
	XDrawLine(Global.display, XtWindow(Global.drawArea), Global.xorGC,
	    lastx, starty, startx, starty);
        break;
    }
    if ( mode != XGD_BAND_END ) {
	XDrawLine(Global.display, XtWindow(Global.drawArea), Global.xorGC,
	    startx, starty, startx, *y);
	XDrawLine(Global.display, XtWindow(Global.drawArea), Global.xorGC,
	    startx, *y, *x, *y);
	XDrawLine(Global.display, XtWindow(Global.drawArea), Global.xorGC,
	    *x, *y, *x, starty);
	XDrawLine(Global.display, XtWindow(Global.drawArea), Global.xorGC,
	    *x, starty, startx, starty);
    }
}

void 
#ifdef _NO_PROTO
CreateXorGc()
#else
CreateXorGc(void)
#endif
{
    XGCValues gcv;
    Pixel white = WhitePixel(Global.display, Global.screenNo);
    Pixel black = BlackPixel(Global.display, Global.screenNo);

    gcv.function = GXxor;
    gcv.foreground = 0xffffffff;
    gcv.plane_mask = black ^ white;
    Global.xorGC = XCreateGC(Global.display, XtWindow(Global.drawArea),
        GCPlaneMask|GCForeground|GCFunction, &gcv);
}
