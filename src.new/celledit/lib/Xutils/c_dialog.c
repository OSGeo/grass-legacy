/***********************************************************************

File     	:	c_dialog.c
Function 	:	CreateDialog(str1, int1,)
Args	 	:	    char *str1; -- return string
  	    		    int int1; -- extraction source

Author   	:	Frank Goodman -- spanki@ced.berkeley.edu
Creation 	:	18 January 1990
Last Revised	:	05 April 1990 -- spanki@ced.berkeley.edu
Abstract 	:	Extracts and returns a substring from a raw string.
Returns  	:	None.

***********************************************************************/
#include <stdio.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/Dialog.h>

Widget CreateDialogBox(parent, name)
    Widget parent;
    char *name;
    {
    Widget dialog;

    dialog = XtVaCreateManagedWidget(name, dialogWidgetClass, parent,
					XtNvalue, (XtArgVal)"", NULL);
    return (dialog);
    }
