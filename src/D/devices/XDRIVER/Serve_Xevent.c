#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <stdio.h>
	
extern XFontStruct *fontstruct;
extern char *grasstr;
extern Display *dpy;
extern Window  grwin;
extern Pixmap  bkupmap;
extern GC      gc;
extern Colormap grasscmap;
extern unsigned SC_WID;
extern unsigned SC_HITE;

static int firstime = 1;

Service_Xevent(wait)
int wait;
{
    XEvent event;

    /*
     * If wait is zero wait for and service the next X event. If wait 
     * is non-zero see if any events are ready. If none just return.
     */
    
    if ( wait != 0 ) {
	if (!XPending(dpy))
	   return;  		/* no events in queue, return */
	XNextEvent(dpy, &event);
    }

    /*
     * On the first Expose events, write the grass display message.
     * For now, on subsequent expose copy the backup window to
     * the display window.
     */
    if ((wait == 0) || (event.type == Expose && event.xexpose.count == 0))
    {
	XWindowAttributes xwa;	/* Temp Get Window Attribute struct */
	int         x, y;
	/*
	 * Remove any other pending Expose events from the queue to
	 * avoid multiple repaints.
	 */
	while (XCheckTypedEvent(dpy, Expose, &event));
	    
	/*
	 * Get the window's current attributes.
	 */
	if (XGetWindowAttributes(dpy, grwin, &xwa) == 0)
	    return;

	if (firstime) {
	    x = (xwa.width - 
		 XTextWidth(fontstruct, grasstr, strlen(grasstr))) / 2;
	    y = (xwa.height + fontstruct->max_bounds.ascent
		 - fontstruct->max_bounds.descent) / 2;
	    /*
	     * Fill the window with the background color,  and then paint
	     * the centered string.
	     */
	    XClearWindow(dpy, grwin);
	    XDrawString(dpy, grwin, gc, x, y, grasstr, strlen(grasstr));
	    firstime = 0;
	    }
	else {
	    XCopyArea(dpy,bkupmap,grwin,gc,0,0,SC_WID,SC_HITE,0,0);
	}
    }
    /* When the pointer enters the GRASS display window we need
     * to switch to the grass colormap. Likewise, when we exit we
     * switch back to the default. Some window managers will take
     * care of this for us, but we cannot depend on a manager.
     */
    else if (event.type == EnterNotify)
	XInstallColormap(dpy,grasscmap);
/*
    else if (event.type == LeaveNotify)
	XInstallColormap(dpy,DefaultColormap(dpy,DefaultScreen(dpy)));
*/
}
