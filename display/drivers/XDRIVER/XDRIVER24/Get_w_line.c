/* Using mouse device, get a new screen coordinate and button number.
 * Button numbers must be the following values which correspond to the
 * following software meanings: 1 - left button 2 - middle button 3 -
 * right button
 * 
 * This is called directly by the application programs.
 * 
 * A "rubberband" line is used.  One end is fixed at the (cx, cy)
 * coordinate. The opposite end starts out at (*nx, *ny) and then
 * tracks the mouse. Upon button depression, the current coordinate is
 * returned in (*nx, *ny) and the button pressed in returned in
 * *button. */

#include "gis.h"
#include "includes.h"

/* Returns: -1  error
*            0
*            1 for cmd = continue : button pressed
*/
int Get_location_with_line2 (
    int cx, int cy,                     /* current x and y */
    int *nx, int *ny,                   /* new x and y */
    int *button,
    int cmd          /* 1 start, 2, continue, 3 stop */
)
{
    static int drawn = 0;
    static long event_mask;
    static int oldx, oldy;
    XEvent event;
    static GC xor_gc;
    XGCValues gcValues;
    unsigned gcMask;

    if (redraw_pid)
    {
	fprintf(stderr, "Monitor: interactive command in redraw\n");
	return -1;
    }

    G_debug (4, "Get_location_with_line2(): cmd = %d", cmd );
    if ( cmd == 1 ) {
	drawn = 0;
	/* Set the crosshair cursor */
	XDefineCursor(dpy, grwin, cur_xh);
	
	/* XOR, so double drawing returns pixels to original state */
	gcMask = GCFunction | GCPlaneMask | GCForeground | GCLineWidth;
	gcValues.function = GXxor;
	gcValues.line_width = 1;
	gcValues.plane_mask = BlackPixel(dpy,scrn)^WhitePixel(dpy,scrn);
	gcValues.foreground = 0xffffffff;
	xor_gc = XCreateGC(dpy,grwin,gcMask,&gcValues);

	/* Get events that track the pointer to resize the RubberBox until
	 * ButtonReleased */
	event_mask = ButtonPressMask | PointerMotionMask;
	XSelectInput(dpy, grwin, event_mask);

	return 0;
    }

    if ( cmd == 2 ) {
	if ( XCheckWindowEvent(dpy, grwin, event_mask, &event) ) { /* get event */
	    switch (event.type) {
		case ButtonPress:
		    *button = event.xbutton.button;
		    *nx = event.xbutton.x;
		    *ny = event.xbutton.y;
		    break;

		case MotionNotify:
		    *nx = event.xbutton.x;
		    *ny = event.xbutton.y;
		    /* do a double draw to 'erase' previous rectangle */
		    if (drawn)
			XDrawLine(dpy, grwin, xor_gc, cx, cy, oldx, oldy);
		    XDrawLine(dpy, grwin, xor_gc, cx, cy, *nx, *ny);
		    oldx = *nx;
		    oldy = *ny;
		    drawn = 1;
		    return 0;
		    break;
	    }
	} else { /* no event -> do nothing */
	    return 0;
	}
    } 
    
    /* This is reached only for 'stop' or 'continue' with button press */

    /* delete old line if any */
    if (drawn) 
	XDrawLine(dpy, grwin, xor_gc, cx, cy, oldx, oldy);

    XUndefineCursor(dpy, grwin);        /* reset cursor */
    XSelectInput(dpy, grwin, gemask);   /* restore normal events */
    XFreeGC(dpy, xor_gc);
    drawn = 0;

    return 1;
}

