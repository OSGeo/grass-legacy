#include "xgdisp.h"


void
#ifdef _NO_PROTO
DrawRulerPixmaps()
#else
DrawRulerPixmaps(void)
#endif
{
    int curX, curY;
    int pWidth, pHeight;
    int i;
    int hXOffset, hYOffset, vXOffset, vYOffset;
    int fHeight;

    XgDoHourGlass(Global.applShell);

    fHeight = Global.rulerFontStruct->max_bounds.ascent;
    hXOffset = 3;
    hYOffset = fHeight;
    vXOffset = 0;
    vYOffset = 2 + fHeight;

    if ( Global.units != XGD_UNITS_MILLI ) {
        /* Get units into 1000th inches */
        pWidth = (int)(Global.pageWidth*1000.0);
        pHeight = (int)(Global.pageHeight*1000.0);

        /* Loop incrementing by 1/8th of an inch */
	for ( i = 0; i <= pWidth; i += 125 ) {
            /* Determine best tick length */
	    int tick;
	    if ( i%1000 == 0 ) tick = 12;
	    else if ( i%500 == 0 ) tick = 8;
	    else if ( i%250 == 0 ) tick = 5;
	    else tick = 3;

            /* Convert X position to pixels and draw */
	    curX  = XmConvertUnits(Global.applShell, XmHORIZONTAL,
				   Xm1000TH_INCHES, i, XmPIXELS);
	    XDrawLine(Global.display, Global.hRuler, Global.rulerGC, 
		curX, 15, curX, 15 - tick);
            if ( i%1000 == 0 || i == 0 ) {
                char buf[5];
                int len;

                if ( i == 0 ) 
                    sprintf(buf,"0");
                else
                    sprintf(buf,"%d", i/1000);
                len = strlen(buf);
                XDrawString(Global.display, Global.hRuler, Global.rulerGC,
                    curX + hXOffset, hYOffset, buf, len);
            }
	}
	for ( i = 0; i <= pHeight; i += 125 ) {
            /* Determine best tick length */
	    int tick;
	    if ( i%1000 == 0 ) tick = 12;
	    else if ( i%500 == 0 ) tick = 8;
	    else if ( i%250 == 0 ) tick = 5;
	    else tick = 3;

            /* Convert Y position to pixels and draw */
	    curY  = XmConvertUnits(Global.applShell, XmVERTICAL,
				   Xm1000TH_INCHES, i, XmPIXELS);
	    XDrawLine(Global.display, Global.vRuler, Global.rulerGC, 
		15, curY, 15 - tick, curY);
            if ( i%1000 == 0 || i == 0 ) {
                char buf[5];
                int len;

                if ( i == 0 ) 
                    sprintf(buf,"0");
                else
                    sprintf(buf,"%d", i/1000);
                len = strlen(buf);
                XDrawString(Global.display, Global.vRuler, Global.rulerGC,
                    vXOffset, curY + vYOffset, buf, len);
            }
	}
    } else {
	/* Get units into 100th of millimeters */
        pWidth = XmConvertUnits(Global.applShell, XmHORIZONTAL,
            Xm1000TH_INCHES, (int)(Global.pageWidth)*1000.0, 
            Xm100TH_MILLIMETERS);
        pHeight = XmConvertUnits(Global.applShell, XmVERTICAL,
            Xm1000TH_INCHES, (int)(Global.pageHeight)*1000.0, 
            Xm100TH_MILLIMETERS);

	for ( i = 0; i <= pWidth; i += 100 ) {
	    int tick;
	    if ( i%1000 == 0 ) tick = 12;
	    else tick = 5;
	    curX  = XmConvertUnits(Global.applShell, XmHORIZONTAL,
				   Xm100TH_MILLIMETERS, i, XmPIXELS);
	    XDrawLine(Global.display, Global.hRuler, Global.rulerGC, 
		curX, 15, curX, 15 - tick);
            if ( i%1000 == 0 || i == 0 ) {
                char buf[5];
                int len;

                if ( i == 0 ) 
                    sprintf(buf,"0");
                else
                    sprintf(buf,"%d", i/1000);
                len = strlen(buf);
                XDrawString(Global.display, Global.hRuler, Global.rulerGC,
                    curX + hXOffset, hYOffset, buf, len);
            }
	}
	for ( i = 0; i <= pHeight; i += 100 ) {
	    int tick;
	    if ( i%1000 == 0 ) tick = 12;
	    else tick = 5;
	    curY  = XmConvertUnits(Global.applShell, XmVERTICAL,
				   Xm100TH_MILLIMETERS, i, XmPIXELS);
	    XDrawLine(Global.display, Global.vRuler, Global.rulerGC, 
		15, curY, 15 - tick, curY);
            if ( i%1000 == 0 || i == 0 ) {
                char buf[5];
                int len;

                if ( i == 0 ) 
                    sprintf(buf,"0");
                else
                    sprintf(buf,"%d", i/1000);
                len = strlen(buf);
                XDrawString(Global.display, Global.vRuler, Global.rulerGC,
                    vXOffset, curY + vYOffset, buf, len);
            }
	}
    }

    XgUndoHourGlass(Global.applShell);
}

void 
#ifdef _NO_PROTO
ClearRulerPixmaps()
#else
ClearRulerPixmaps(void)
#endif
{
    XGCValues gcv;
    Pixel white = WhitePixel(Global.display, Global.screenNo);
    Pixel black = BlackPixel(Global.display, Global.screenNo);

    gcv.function = GXcopy;
    gcv.font = Global.rulerFontStruct->fid;
    gcv.background = black;
    gcv.foreground = white;
    if ( !Global.rulerGC ) {
	Global.rulerGC = XCreateGC(Global.display, Global.hRulerWindow,
	    GCFont|GCForeground|GCBackground|GCFunction, &gcv);
    } else {
	XChangeGC(Global.display, Global.rulerGC,
	    GCFont|GCForeground|GCBackground|GCFunction, &gcv);
    }

    XFillRectangle(Global.display, Global.hRuler, Global.rulerGC, 
        0, 0, Global.hWidth, 15);
    XFillRectangle(Global.display, Global.vRuler, Global.rulerGC, 
        0, 0, 15, Global.vHeight);

    gcv.foreground = black;
    gcv.background = white;
    XChangeGC(Global.display, Global.rulerGC,
        GCForeground|GCBackground|GCFunction, &gcv);
}

void
#ifdef _NO_PROTO
CreateRulers()
#else
CreateRulers(void)
#endif
{
    XWindowAttributes xwa;

    Global.hRulerWindow = XtWindow(Global.hRulerWidget);
    Global.vRulerWindow = XtWindow(Global.vRulerWidget);

    if (XGetWindowAttributes(Global.display, Global.hRulerWindow, &xwa) == 0) {
        fprintf(stderr, "Oops, no window attributes\n");
	exit(1);
    }
    Global.hRuler = XCreatePixmap(Global.display, Global.hRulerWindow,
        Global.hWidth, 15, xwa.depth);
    Global.vRuler = XCreatePixmap(Global.display, Global.vRulerWindow,
        15, Global.vHeight, xwa.depth);
}
