
/* make_position:
** makes a box that appears inset which holds the puck that
** is used to control xy positioning
*/ 

#include "interface.h"

Widget 
make_position (Widget parent, char *name, void (*cb1)(void), void (*cb2)(void), void (*cb3)(void), void (*exposecb)(void), data_cell *dc, Arg wargs[30], int n)
{
    Widget w;
    Widget frame;

    XtSetArg (wargs[n], XmNshadowThickness, 5); n++;
    XtSetArg (wargs[n], XmNshadowType, XmSHADOW_IN); n++; 
    frame = XtCreateManagedWidget ("", xmFrameWidgetClass, parent, wargs, n);
    w = XtCreateManagedWidget (name, xmDrawingAreaWidgetClass, frame, wargs, n);
    
    if(dc){
    	XtAddCallback (w, XmNexposeCallback,exposecb , dc);
    	XtAddEventHandler(w, ButtonPressMask, FALSE, cb1, dc);
    	XtAddEventHandler(w, Button1MotionMask, FALSE, cb2, dc);
	/*
	XtAddEventHandler(w, ButtonReleaseMask, FALSE, cb3, dc);
	*/
    }

    return w;

}


