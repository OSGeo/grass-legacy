/***********************************************************************

File     	:	c_line.c
Function 	:	Widget CreateLine(parent, name )
Args	 	:	    Widget parent; -- parent shell
  	    		    char   *name; -- widget's name

Author   	:	Frank Goodman -- spanki@ced.berkeley.edu
Creation 	:	15 February 1990
Last Revised	:
Abstract 	:	Create an smeBSB line object.
Returns  	:	the new widget.

***********************************************************************/
#include <stdio.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/SmeLine.h>

Widget CreateLine(parent, name)
    Widget parent;
    char *name;
    {
    Widget line;

    line = XtVaCreateManagedWidget(name, smeLineObjectClass, parent, NULL);
    return(line);
    }
