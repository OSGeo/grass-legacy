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

#include "includes.h"
#include "local_proto.h"
#include "../lib/colors.h"


extern Display *dpy;
extern int scrn;
extern Window grwin;
extern Cursor grcurse;
extern GC gc;
extern u_long gemask;

static u_long event_mask;
static int drawn = 0;
static unsigned oldwidth, oldheight;

GC xor_gc;

/* Erase the current line */

static int EraseRubberLine (int x1, int y1, unsigned x2, unsigned y2)
{
	XDrawLine(dpy, grwin, xor_gc, x1, y1, x2, y2);

	return 0;
}


int Get_location_with_line (
    int cx,
    int cy,                     /* current x and y */
    int *nx,
    int *ny,                   /* new x and y */
    int *button
)
{
	XEvent event;
	XGCValues gcValues;
	unsigned gcMask;

	event_mask = PointerMotionMask | ButtonPressMask | ExposureMask;
	XSelectInput(dpy, grwin, event_mask);
	    /* XOR, so double drawing returns pixels to original state */
   gcMask = GCFunction | GCPlaneMask | GCForeground | GCLineWidth ;
   gcValues.function = GXxor;
   gcValues.line_width = 3;
   gcValues.plane_mask = BlackPixel(dpy,scrn)^WhitePixel(dpy,scrn);
   gcValues.foreground = 0xffffffff;
   xor_gc = XCreateGC(dpy,grwin,gcMask,&gcValues);

/*   event_mask |= ExposureMask; */

	while (1) {
		XWindowEvent(dpy, grwin, event_mask, &event);
		/****************
	        while (XCheckWindowEvent(dpy, grwin, event_mask, &event) == False)
	        {
		    Service_Xevent();
		    sleep(1); 
		}
	        ****************/
		switch (event.type) {
		case Expose:
			handleExposeEvent();
			break;
		case MotionNotify:
			*nx = event.xbutton.x;
			*ny = event.xbutton.y;
			if (drawn)
				EraseRubberLine(cx, cy, (int) oldwidth, (int) oldheight);
			XDrawLine(dpy, grwin, xor_gc, cx, cy, *nx, *ny);
			oldwidth = *nx;
			oldheight = *ny;
			drawn = 1;
			break;
		case ButtonPress:
			*nx = event.xbutton.x;
			*ny = event.xbutton.y;
			*button = event.xbutton.button;
			EraseRubberLine(cx, cy, oldwidth, oldheight);
			drawn = 0;
			XSelectInput(dpy, grwin, gemask);
			return 0;
		}
	}
}
