/***********************************************************************

File     	:	c_popup_shell.c
Function 	:	Widget CreatePopupShell(parent, name, type)
Args		:	    Widget parent; -- parent of the new widget
			    char *name; -- the shell's name 
			    int type; -- which kind of shell

Author   	:	Frank Goodman -- spanki@ced.berkeley.edu
Creation 	:	18 January 1990
Last Revised	:	15 February 1990
Abstract 	:       Create a type of popup shell 
Returns  	:       The new shell.

***********************************************************************/
#include <stdio.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Xaw/SimpleMenu.h>
#include <X11/Shell.h>

Widget CreatePopupShell(parent, name, type)
    Widget parent;
    char *name;
    int type;	/* 0 = override, 1 = simpleMenu, 2 = transient */
    {
    Widget shell;
    switch(type)
	{
	case 0:
	    shell = XtVaCreatePopupShell(name, overrideShellWidgetClass,
				     parent, NULL);
	    break;
    	case 1:
	    shell = XtVaCreatePopupShell(name, simpleMenuWidgetClass,
					 parent, NULL);
	    break;
	case 2:
	    shell = XtVaCreatePopupShell(name, transientShellWidgetClass,
					 parent, NULL);
	    break;
	}
    return(shell);
    }

void MovePopup(w, x, y)
    Widget w;
    int x, y;
    {
    Arg args[2];
    XtSetArg(args[0], XtNx, (XtArgVal)x);
    XtSetArg(args[1], XtNy, (XtArgVal)y);
    XtSetValues(w, args, 2);
    }
