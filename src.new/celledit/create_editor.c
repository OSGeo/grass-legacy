/***********************************************************************

File     	:	create_editor.c
Function 	:	Widget CreateEditor(parent)
Args	 	:	    Widget parent; -- parent of the form

Author   	:	Frank Goodman -- spanki@ced.berkeley.edu
Creation 	:	27 January 1990
Last Revised	:
Abstract 	:	create an instance of the editor widget
			of out topLevel shell.
Returns  	:	None.
***********************************************************************/
#include "cell_editor.h"
#include "CellEdit.h"

Widget CreateEditor(parent)
    Widget parent;
    {
    Widget edit;

    edit = XtCreateManagedWidget("editor", cellEditWidgetClass, parent, 
				 NULL, 0); 
    return(edit);
    }
