/***********************************************************************

File     	:	c_bsb.c
Function 	:	Widget CreateBSB(parent,name,label,callback)
Args	 	:	    Widget parent; -- parent shell
  	    		    char   *name; -- widget's name
  	    		    char   *label; -- widget's label

Author   	:	Frank Goodman -- spanki@ced.berkeley.edu
Creation 	:	15 February 1990
Last Revised	:
Abstract 	:	Create an smeBSB object.
Returns  	:	the new widget.

***********************************************************************/
#include <stdio.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/SmeBSB.h>

Widget CreateBSB(parent, name)
    Widget parent;
    char *name;
    {
    Widget bsb;

    bsb = XtVaCreateManagedWidget(name, smeBSBObjectClass, parent, NULL);
    return(bsb);
    }
