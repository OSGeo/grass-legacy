/***********************************************************************

File     	:	c_label.c
Function 	:	Widget CreateLabel(parent, name, label)
Args	 	:	    Widget parent; -- label's parent
			    char *name; --  label's name
Author   	:	Frank Goodman -- spanki@ced.berkeley.edu
Creation 	:	30 January 1990
Last Revised	:
Abstract 	:	Create a label
Returns  	:	None.
***********************************************************************/
#include <stdio.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/Label.h>

Widget CreateLabel( parent, name )
    Widget parent;
    char *name;
    {
    Widget labelWidget;

    labelWidget= XtVaCreateManagedWidget(name, labelWidgetClass, parent, 
				      NULL);
    return(labelWidget);
    }   

/* change the labels string */
void SetLabelStr(w, str)
    Widget w;
    char *str;
    {
    Arg labelArg[1];
    XtSetArg(labelArg[0], XtNlabel, (XtArgVal)str);
    XtSetValues(w, labelArg, (Cardinal)1);
    XFlush(XtDisplay(w));
    }
