/***********************************************************************

File     	:	c_menu_btn.c
Function 	:	Widget CreateMenuButton(parent, name, shell)
Args	 	:	    Widget parent; -- menu's parent
  	    		    char *name; -- menu's name
			    char *shell; -- the associated shell. 

Author   	:	Frank Goodman -- spanki@ced.berkeley.edu
Creation 	:	18 January 1990
Last Revised	:	28 January 1990
Abstract 	:	Create a menu button.
Returns  	:	the new button.

***********************************************************************/
#include <stdio.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/MenuButton.h>
#include <X11/Xaw/Form.h>

Widget CreateMenuButton(parent, name, shell)
    Widget parent;
    char *name;
    char *shell;
    {
    Widget menuButton;
    menuButton = 
	XtVaCreateManagedWidget(name, menuButtonWidgetClass, parent,
				  XtNmenuName, (XtArgVal)shell, NULL);
    return(menuButton);
    }
