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

#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

extern Display *dpy;
extern Window grwin;
extern Cursor grcurse;
extern GC gc;
extern u_long gemask;

static u_long event_mask;
static int drawn = 0;
static unsigned oldwidth, oldheight;

/* Erase the current line */

static EraseRubberLine(x1, y1, x2, y2)
int x1, y1, x2, y2;
{
    if (drawn)
        XDrawLine(dpy, grwin, gc, x1, y1, x2, y2);
    drawn = 1;
}


Get_location_with_line(cx, cy, nx, ny, button)
int cx, cy;                     /* current x and y */
int *nx, *ny;                   /* new x and y */
int *button;
{
    XEvent event;

    event_mask = PointerMotionMask | ButtonPressMask;
    XSelectInput(dpy, grwin, event_mask);
    XSetFunction(dpy, gc, GXxor);

    while (1) {
        XWindowEvent(dpy, grwin, event_mask, &event);
        switch (event.type) {
        case MotionNotify:
            *nx = event.xbutton.x;
            *ny = event.xbutton.y;
            if (drawn)
                EraseRubberLine(cx, cy, (int) oldwidth, (int) oldheight);
            XDrawLine(dpy, grwin, gc, cx, cy, *nx, *ny);
            oldwidth = *nx;
            oldheight = *ny;
            drawn = 1;
            break;
        case ButtonPress:
            *nx = event.xbutton.x;
            *ny = event.xbutton.y;
            *button = event.xbutton.button;
            EraseRubberLine(cx, cy, (int) oldwidth, (int) oldheight);
            XSetFunction(dpy, gc, GXcopy);
            drawn = 0;
            XSelectInput(dpy, grwin, gemask);
            return 0;
        }
    }
}
