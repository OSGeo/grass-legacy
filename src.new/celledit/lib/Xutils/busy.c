/***********************************************************************

File     	:	create_busy.c
Function 	:	Window CreateBusyWindow(parent)
Args	 	:	    parent; -- parent

Author   	:	Frank Goodman -- spanki@ced.berkeley.edu
Creation 	:	24 February 1990
Last Revised	:
Abstract 	:	Creates an InputOnly window with a destinct
			cursor for use when we're busy.
Returns  	:	None.

***********************************************************************/
#include <X11/Intrinsic.h>
#include <X11/cursorfont.h>

Window CreateBusyWindow(parent)
    Widget parent;
    {
    int 			screen;
    unsigned long               valuemask;
    XSetWindowAttributes       	attributes;
    Window 			pWin;
    unsigned int 		w;
    unsigned int 		h;
    Display 			*dpy;
    Window 			busy;

    dpy = 	XtDisplay(parent);
    screen =	DefaultScreen(dpy);
    pWin =	XtWindow(parent);
    w =         DisplayWidth(dpy, screen);
    h =         DisplayHeight(dpy, screen);

    valuemask = CWDontPropagate | CWCursor;

    attributes.do_not_propagate_mask = (KeyPressMask    | KeyReleaseMask |
				        ButtonPressMask | ButtonReleaseMask |
					PointerMotionMask);

    attributes.cursor = XCreateFontCursor(dpy, XC_watch);

    /* since our parent window is our first widget (form)
     * this call will create a subwindow of form, that is 
     * the size of the root. Since every child window must
     * be contained within it's parent, this should
     * clip our busy window to the size of our form,
     * This buys us free resizes.
     */
    busy = XCreateWindow(dpy, pWin, 0, 0, w, h, (unsigned int)0, 
			 CopyFromParent, InputOnly, CopyFromParent,
			 valuemask, &attributes);
    return(busy);
    }

/* map the busy window */
void ShowBusy(dpy, win)
    {
    XMapWindow(dpy, win);
    XFlush(dpy);
    }

/* unmap the busy window */
void HideBusy(dpy, win)
    {
    XUnmapWindow(dpy, win);
    XFlush(dpy);
    }
