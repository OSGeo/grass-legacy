#include "digit.h"

Check_for_interrupt()
{
    XEvent event;


    return (XCheckWindowEvent (XtDisplay (Cancel), XtWindow (Cancel),
					   ButtonPressMask, &event));

}

TimeOutCursor (on)
   int on;
{
    Display *display = XtDisplay (toplevel);
    Window win = XtWindow (toplevel);
    static int locked = 0;
    static Cursor cursor, cursor2;
    XSetWindowAttributes attrs;
    XEvent event;
    on? locked++ : locked--;
    if (locked>1 || locked == 1 && on == 0)
	return;
    if (!cursor && on)
	cursor =XCreateFontCursor (display, XC_watch);
    if (!cursor2 && on)
	cursor2 =XCreateFontCursor (display, XC_arrow);


    attrs.cursor = on? cursor : None;
    XChangeWindowAttributes (display, win, CWCursor, &attrs);
 

    attrs.cursor = on? cursor2 : None;
    XChangeWindowAttributes (display, XtWindow (Cancel), CWCursor, &attrs);

}


