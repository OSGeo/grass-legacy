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

#include <X11/Xos.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/cursorfont.h>
#include "../lib/colors.h"

#ifdef SYSV
typedef unsigned long u_long; 
#endif

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
static GC xor_gc;

/* Erases the current RubberBox */
static EraseRubberBox(x1, y1, x2, y2)
int x1, y1;
unsigned x2, y2;
{
    if (drawn)
        XDrawRectangle(dpy, grwin, xor_gc, x1, y1, x2, y2);
    drawn = 1;
}


Get_location_with_box(cx, cy, nx, ny, button)
int cx, cy;                     /* current x and y */
int *nx, *ny;                   /* new x and y */
int *button;
{
    XEvent event;
    int leftx, topy;
    XGCValues gcValues;
    unsigned gcMask;

    /* Get events that track the pointer to resize the RubberBox until
     * ButtonReleased */
    event_mask = ButtonPressMask | PointerMotionMask; 
    XSelectInput(dpy, grwin, event_mask);

    /* XOR, so double drawing returns pixels to original state */
    gcMask = GCFunction | GCPlaneMask | GCForeground ;
   gcValues.function = GXxor;
   gcValues.plane_mask = AllPlanes;
   gcValues.foreground = _get_color_index(WHITE);
   xor_gc = XCreateGC(dpy,grwin,gcMask,&gcValues);


    /* Set the crosshair cursor */
    XDefineCursor(dpy, grwin, grxh);

    while (1) {
        XWindowEvent(dpy, grwin, event_mask, &event);

        switch (event.type) {
        case ButtonPress:
            *button = event.xbutton.button;
            *nx = event.xbutton.x;
            *ny = event.xbutton.y;
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
}
