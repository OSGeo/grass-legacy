/***********************************************************************

File     	:	create_list.c
Function 	:	Widget CreateList(parent, list, callback)
Args	 	:	    Widget parent; -- list's parent
			    Widget list; --  list to use
			    Widget callback; -- callback Rec
Author   	:	Frank Goodman -- spanki@ced.berkeley.edu
Creation 	:	18 January 1990
Last Revised	:	28 January 1990
Abstract 	:	Create a list widget
Returns  	:	None.
***********************************************************************/
#include "cell_editor.h"
#include <X11/Xaw/List.h>

static char *listTrans = 
    "<BtnUp>:		Set() Notify() Unset() \n\
     <BtnMotion>:	Set() \n\
     <LeaveWindow>:	Set() PopDown()";

static Arg listArgs[]= 
    {
	{XtNlist,  		(XtArgVal)NULL},
 	{XtNcallback,		(XtArgVal)NULL},
 	{XtNtranslations,	(XtArgVal)NULL},
    };

static Widget listWidget;
Widget CreateList(parent, list, callback)
    Widget parent;
    char **list;
    XtCallbackRec *callback;
    {
    XtTranslations trans = XtParseTranslationTable(listTrans);

    XtSetArg(listArgs[0], XtNlist, (XtArgVal)list);
    XtSetArg(listArgs[1], XtNcallback, (XtArgVal)callback);
    listWidget = XtCreateManagedWidget("list", listWidgetClass, parent, 
				       listArgs, XtNumber(listArgs) );
    XtSetArg(listArgs[2], XtNtranslations, (XtArgVal)trans);
    XtSetValues(listWidget, listArgs, XtNumber(listArgs));
    return(listWidget);
    }

void PopDown(w, client, calls)
    Widget w;
    XtPointer client;
    XtPointer calls;
    {
    XtPopdown(XtParent(w));
    }

void NewList(list)
    char **list;
    {
    Arg arg[1];
    XtSetArg(arg[0], XtNlist, list);
    XtSetValues(listWidget, arg, 1);
    }
    
