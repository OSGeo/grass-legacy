/* Using mouse device, get a new screen coordinate and button number.
 * Button numbers must be the following values which correspond to the
 * following software meanings: 1 - left button 2 - middle button 3 -
 * right button
 * 
 * This is called directly by the application programs.
 * 
 * A "pointing hand" pointer is used. Upon button depression, the current
 * coordinate is returned in (*wx, *wy) and the button pressed in
 * returned in *button. */

#include "gis.h"
#include "includes.h"
#include "local_proto.h"
#include "glocale.h"

int Get_location_with_pointer2 (int *wx, int *wy, int *button, int cmd)
{
    XEvent event;
    
    if (redraw_pid)
    {
	fprintf(stderr, _("Monitor: interactive command in redraw\n"));
	return -1;
    }

    G_debug (5, "Get_location_with_pointer2(): cmd = %d", cmd ); 
    
    /* wait for a button-push event in the grass window, and return the
     * x,y coord and button number */

    if(*button == -1){
	/* How this is used?
	 * When *button = -1 is given, this function immediately returns
	 * without waiting for an event. This is mainly used to get
	 * the location of the pointer with or without a button-push event.
	 * If no button is pushed, *button = -1 is returned.
	 */
        u_int  mask;
        Window root, child;
        int    rx, ry;

        XDefineCursor(dpy, grwin, cur_xh);
        XGetInputFocus(dpy, &child, &rx);
	if(child == grwin){
	    XQueryPointer(dpy, grwin, &root, &child, &rx, &ry, wx, wy, &mask);
	    *button = (mask&Button1Mask ? 1
			    : (mask&Button2Mask ? 2
				    : (mask&Button3Mask ? 3
					    : *button)));
	}else{
	    *wx = *wy = -1;
	}
        XUndefineCursor(dpy, grwin);
        return 1;
    }else{
        if ( cmd == 1 ) { 
	    XDefineCursor(dpy, grwin, cur_xh);
	    XSelectInput(dpy, grwin, ButtonPressMask | PointerMotionMask );
	    return 0;
	}

	if ( cmd == 2 ) {
	    if ( XCheckWindowEvent(dpy, grwin, ButtonPressMask | PointerMotionMask, &event) ) {
		*wx = event.xbutton.x;
		*wy = event.xbutton.y;
		if ( event.type == ButtonPress ) {
		    *button = event.xbutton.button;
		} else { /* Motion */
		    return 0;
		}
	    } else {  /* no event -> do nothing */ 
		return 0;
	    }
	}

	if ( ( cmd == 2 && *button > 0 ) || cmd == 3 ) {
            XUndefineCursor(dpy, grwin);
	    XSelectInput(dpy, grwin, gemask);
        }
	    
	return 1;
    }

    return 1;
}
