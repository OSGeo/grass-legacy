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

#include "includes.h"
#include "local_proto.h"

int Get_location_with_pointer (int *wx, int *wy, int *button)
{
    if (redraw_pid)
    {
	fprintf(stderr, "Monitor: interactive command in redraw\n");
	return -1;
    }

    /* set the grass cursor on (defined in Graph_Set.c) */
    XDefineCursor(dpy, grwin, cur_xh);

    /* wait for a button-push event in the grass window, and return the
     * x,y coord and button number */

    if(*button == -1){
        u_int  mask;
        Window root, child;
        int    rx, ry;

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
    }else{
        XEvent bpevent;

	XSelectInput(dpy, grwin, ButtonPressMask);

	do 
	{
	    if (!Get_Xevent(ButtonPressMask, &bpevent))
		break;
	} while (bpevent.type != ButtonPress);

	XSelectInput(dpy, grwin, gemask);

	*wx = bpevent.xbutton.x;
	*wy = bpevent.xbutton.y;
	*button = bpevent.xbutton.button;
    }

    XUndefineCursor(dpy, grwin);

    return 0;
}
