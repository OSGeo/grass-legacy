/***********************************************************************

File     	:	c_form.c
Function 	:	Widget CreateForm(parent, name)
Args	 	:	    Widget parent; -- parent of the form
			    char *name; - forms name.

Author   	:	Frank Goodman -- spanki@ced.berkeley.edu
Creation 	:	18 January 1990
Last Revised	:
Abstract 	:	Create a form widget as the only child
			of out topLevel shell.
Returns  	:	None.
***********************************************************************/
#include <stdio.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/Form.h>

Widget CreateForm(parent, name)
    Widget parent;
    char *name;
    {
    Widget form;
    form = XtVaCreateManagedWidget(name, formWidgetClass, parent, NULL);
    return(form);
    }
