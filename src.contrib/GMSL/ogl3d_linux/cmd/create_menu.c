
/* xman_create_menu_items:
** this is the source code for creating menu panes. all calls to this       
** function must take the form                                             
** xman_create_menu_item(char title, Widget parent,                       
**                       xman_menu_struct menulist[],int nitems);        
*/


#include "interface.h"

void 
xman_create_menu_items (char *title, Widget menu, xman_menu_struct *menulist, int nitems)
{
    Arg wargs[1];
    int i;
    WidgetList buttons;
    int separators = 0;
    XmString tmp;

    /* allocate a widget list to hold all button widgets */

    buttons = (WidgetList) XtMalloc(nitems * sizeof(Widget));

    /* if a title is given, create Label and Separator widgets */
    if(title){ 
	XtCreateManagedWidget(title, xmLabelWidgetClass, menu, NULL, 0);
	XtCreateManagedWidget("separator", xmSeparatorWidgetClass, 
		menu, NULL, 0);

    }

    /* Create an entry for each item in the menu. 
    A null name represents a separator */

    for(i = 0; i < nitems; i++){

	if(menulist[i].name == NULL){
	    XtCreateManagedWidget("separator", xmSeparatorWidgetClass, 
		    menu, NULL, 0);
	    separators++; /* count how many entries aren't buttons */
	}


	/* if there is a name and a callback, 
	create a selectable menu entry and register the callback function */

	else if(menulist[i].func){
	    tmp = XmStringCreate(menulist[i].name, XmSTRING_DEFAULT_CHARSET);
	    XtSetArg(wargs[0], XmNlabelString, tmp);
	    buttons[i - separators] = XtCreateWidget(menulist[i].name, 
		    xmPushButtonWidgetClass, menu, wargs,1);
	    XtAddCallback(buttons[i - separators],XmNactivateCallback, 
		    menulist[i].func,menulist[i].data);
	}

	/* if there is a name, but no callback function, 
	the entry must be a label,unless there is a submenu */

	else if(!menulist[i].sub_menu){
	     XtSetArg(wargs[0],XmNalignment,XmALIGNMENT_BEGINNING);
	     buttons[i - separators] = XtCreateWidget(menulist[i].name, 
		     xmLabelWidgetClass, menu, wargs,1);
	}

	/* If we got here, then the entry must be a submenu. 
	 1) Create a Pulldown menu pane and an XmCascadeButton widget
	 2) Attach the menu pane and make a recursive call to create the 
	    entries in the submenu. */ 

	else{
	    Widget sub_menu;
	    sub_menu = XmCreatePulldownMenu(menu, 
		    menulist[i].sub_menu_title, NULL,0);
	    XtSetArg(wargs[0], XmNsubMenuId, sub_menu);
	    buttons[i-separators]=XtCreateWidget(menulist[i].name, 
		    xmCascadeButtonWidgetClass, menu, wargs,1);
	    xman_create_menu_items(menulist[i].sub_menu_title, sub_menu, 
		    menulist[i].sub_menu, menulist[i].n_sub_items);
	}
    }

    /* manage all button widgets. Menu panes are not managed */

    XtManageChildren(buttons, nitems-separators);

}



