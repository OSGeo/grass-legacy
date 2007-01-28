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

#include <grass/gis.h>
#include "includes.h"
#include <grass/glocale.h>

/* Returns: -1  error
*            0
*            1 for cmd = continue : button pressed
*/
int XD_Get_location_with_line (
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
    int done;

    if (redraw_pid)
    {
	G_warning( _("Monitor: interactive command in redraw"));
	return -1;
    }

    G_debug (5, "Get_location_with_line2(): cmd = %d", cmd );
    if ( cmd == 1 || cmd == 4 ) {
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
    }

    if ( cmd == 1 )
	return 0;

    if ( cmd == 2 || cmd == 4 ) {
        for (done = 0; !done; ) {
	    if ( cmd == 2 && !get_xevent(event_mask, &event, 0) ) {
		return 0;
            }

	    if ( cmd == 4 && !get_xevent(event_mask, &event, 1)) {
		break;
            }

	    switch (event.type) {
		case ButtonPress:
		    *button = event.xbutton.button;
		    *nx = event.xbutton.x;
		    *ny = event.xbutton.y;
		    done = 1;
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
		    if ( cmd == 2 ) return 0;
		    break;
	    }
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

