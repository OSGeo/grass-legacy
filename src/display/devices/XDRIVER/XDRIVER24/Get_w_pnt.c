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
    XEvent bpevent;

    if (redraw_pid)
    {
	fprintf(stderr, "Monitor: interactive command in redraw\n");
	return -1;
    }

    /* set the grass cursor on (defined in Graph_Set.c) */
    XDefineCursor(dpy, grwin, cur_xh);

    /* wait for a button-push event in the grass window, and return the
     * x,y coord and button number */

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

    XUndefineCursor(dpy, grwin);

    return 0;
}
