/* Using mouse device, get a new screen coordinate and button number.
 * Button numbers must be the following values which correspond to the
 * following software meanings: 1 - left button 2 - middle button 3 -
 * right button
 * 
 * This is called directly by the application programs.
 * 
 * A "rubberband" box is used.  One corner is fixed at the (cx, cy)
 * coordinate.  The opposite coordinate starts out at (*nx, *ny) and
 * then tracks the mouse.  Upon button depression, the current
 * coordinate is returned in (*nx, *ny) and the button pressed is
 * returned in button. */

#include "includes.h"
#include <X11/cursorfont.h>
#include "../lib/colors.h"

extern Display *dpy;
extern int scrn;
extern Window grwin;
extern Cursor grcurse, grxh;
extern GC gc;
extern u_long gemask;           /* normal event mask */

static int drawn = 0;
static u_long event_mask;
static unsigned width, height, oldwidth, oldheight;
static int oldx, oldy;
GC xor_gc;

/* Erases the current RubberBox */
static int EraseRubberBox (int x1, int y1, unsigned x2, unsigned y2)
{
    XDrawRectangle(dpy, grwin, xor_gc, x1, y1, x2, y2);

    return 0;
}

int Get_location_with_box (
    int cx,
    int cy,                     /* current x and y */
    int *nx,
    int *ny,                   /* new x and y */
    int *button
)
{
    XEvent event;
    int leftx, topy;
    XGCValues gcValues;
    unsigned gcMask;

    /* Get events that track the pointer to resize the RubberBox until
     * ButtonReleased */
    event_mask = ButtonPressMask | PointerMotionMask | ExposureMask; 
    XSelectInput(dpy, grwin, event_mask);

    /* XOR, so double drawing returns pixels to original state */
    gcMask = GCFunction | GCPlaneMask | GCForeground | GCLineWidth;
   gcValues.function = GXxor;
   gcValues.line_width = 0;
   gcValues.plane_mask = BlackPixel(dpy,scrn)^WhitePixel(dpy,scrn);
   gcValues.foreground = 0xffffffff;
   xor_gc = XCreateGC(dpy,grwin,gcMask,&gcValues);


    /* Set the crosshair cursor */
    XDefineCursor(dpy, grwin, grxh);

    while (1) {
        XWindowEvent(dpy, grwin, event_mask, &event);
	/*********************
	while (XCheckWindowEvent(dpy, grwin, event_mask, &event) == False)
	{
	    Service_Xevent();
	    sleep(1); 
	}
	**********************/
        switch (event.type) {
	case Expose:
	    handleExposeEvent();
	    break;
        case ButtonPress:
            *button = event.xbutton.button;
            *nx = event.xbutton.x;
            *ny = event.xbutton.y;
            if ( drawn ) 
				EraseRubberBox(oldx, oldy, oldwidth, oldheight);
            XDefineCursor(dpy, grwin, grcurse); /* reset cursor */
            drawn = 0;
            XSelectInput(dpy, grwin, gemask);   /* restore normal events */
            return 0;

        case MotionNotify:
            *nx = event.xbutton.x;
            *ny = event.xbutton.y;
            /* do a double draw to 'erase' previous rectangle */
            if (drawn)
                EraseRubberBox(oldx, oldy, oldwidth, oldheight);
            /* need to draw a rectangle with (cx,cy) as one corner and
             * (*nx,*ny) as opposite corner. Figure the top left coords
             * of such a rectangle */
            if (cx < *nx) {
                leftx = cx;
                width = *nx - cx;
            } else {
                leftx = *nx;
                width = cx - *nx;
            }
            if (cy < *ny) {
                topy = cy;
                height = *ny - cy;
            } else {
                topy = *ny;
                height = cy - *ny;
            }
            /* don't draw a zero volumn rectangle */
            if (width && height) {
                XDrawRectangle(dpy, grwin, xor_gc, leftx, topy, width, height);
                oldwidth = width;
                oldheight = height;
                oldx = leftx;
                oldy = topy;
                drawn = 1;
            } else
                drawn = 0;
            break;

        }
    }

    return 0;
}
