/***********************************************************************

File     	:	c_viewport.c
Function 	:	Widget CreateViewport(parent, name)
Args	 	:	    Widget parent; -- viewport's parent
			    char *name; --  viewport's name
Author   	:	Frank Goodman -- spanki@ced.berkeley.edu
Creation 	:	18 January 1990
Last Revised	:	28 January 1990
Abstract 	:	Create the viewport
Returns  	:	the new viewport
***********************************************************************/
#include <stdio.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/Viewport.h>

Widget CreateViewport(parent, name )
    Widget parent;
    {
    Widget view;
    view= XtVaCreateManagedWidget(name, viewportWidgetClass, parent, 
				      NULL);
    return(view);
    }   
