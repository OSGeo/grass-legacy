/***********************************************************************

File     	:	c_button.c
Function 	:	void CreateButton(parent, name)
			    Widget parent; -- parent
			    char   *name; -- quit name

Author   	:	Frank Goodman -- spanki@ced.berkeley.edu
Creation 	:	18 January 1990
Last Revised	:	23 February 1990
Abstract 	:	Create button
Returns  	:	None.

***********************************************************************/
#include <stdio.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/Command.h>

Widget CreateButton(parent, name)
    Widget parent;
    char   *name;
    {
    Widget button;
    button = XtVaCreateManagedWidget(name, commandWidgetClass, parent, NULL);
    return(button);
    }
