static char rcsid[] = "@(#)XGRASS $Id: create.c,v 0.0.0.1 1992/05/05 14:58:25 kurt Exp kurt $";
/*
* File: create.c
*
* Desc: contains code to create menu system
*
* Auth: Kurt Buehler
*
* Date: Thu Oct 17 12:20:10 CDT 1991
*
* Modification History:
*
*
*/

#include "xgrass.h"

CreateMenuSystem()
{
    XgMenuBarListRec *mbptr = _XG_Global.menuData.menuBarList;
    Widget frame;
    Widget menubar;
    Widget pulldown;

    /* create a frame for main menu bar */
    frame = XtVaCreateManagedWidget("Frame", xmFrameWidgetClass,
        _XG_Global.applShell,NULL);
    /* create the main menu bar */
    menubar = XmCreateMenuBar(frame,"MenuBar",NULL,0);

    /* now create the main menu bar items, and sub menu system */
    while( mbptr ) {
        (void)BuildItem(menubar, mbptr->menubar_item->submenu_type,
            mbptr->menubar_item->label,
            mbptr->menubar_item->mnemonic,
            mbptr->menubar_item->accelerator,
            mbptr->menubar_item);
        mbptr = mbptr->next;
    }
    XtManageChild(menubar);
}

void PopupTearOff(w, cld, cad)
Widget w;
XtPointer cld, cad;
{
    Widget popup = (Widget)cld;

    XtPopup(popup,XtGrabNone);
}

Widget 
BuildItem(parent, type, name, mnemonic, accelerator, ptr)
Widget parent;
int type;
char *name;
char *mnemonic;
char *accelerator;
XgMenuItemRec *ptr;
{
    int i;
    Widget menu;
    Widget cascade;
    Widget sidebar;
    XmString xmstring;

    if ( type != XG_MENU_TEAR_OFF_H && type != XG_MENU_TEAR_OFF_V && 
	 type != XG_MENU_NONE && type != XG_MENU_HACK ) {
        /* create pulldown menu */
        menu =  XmCreatePulldownMenu(parent, ptr->submenu_label, NULL, 0);
    }

    if ( type == XG_MENU_PULLDOWN || type == XG_MENU_PULLRIGHT ) {
        /* create cascade button */
        xmstring = XmStringCreateSimple(name);
        cascade = XtVaCreateManagedWidget(name,
            xmCascadeButtonWidgetClass, parent,
            XmNlabelString, xmstring,
            XmNsubMenuId, menu,
            NULL);
        XmStringFree(xmstring);
	if ( ptr->mnemonic ) {
	    XtVaSetValues(cascade,XmNmnemonic,ptr->mnemonic[0],NULL);
	}
	if ( ptr->accelerator ) {
	    XtVaSetValues(cascade,XmNaccelerator,ptr->accelerator,NULL);
	}
    } else if ( type == XG_MENU_HACK ) {
        /* create push button for *the* hack */
        xmstring = XmStringCreateSimple(name);
        cascade = XtVaCreateManagedWidget(name,
            xmCascadeButtonWidgetClass, parent,
            XmNlabelString, xmstring,
            NULL);
        XmStringFree(xmstring);
    } else if ( type == XG_MENU_TEAR_OFF_H || type == XG_MENU_TEAR_OFF_V ) {
        /* create push button for tear off */
        xmstring = XmStringCreateSimple(name);
        cascade = XtVaCreateManagedWidget(name,
            xmCascadeButtonWidgetClass, parent,
            XmNlabelString, xmstring,
            NULL);
        XmStringFree(xmstring);
	if ( ptr->mnemonic ) {
	    XtVaSetValues(cascade,XmNmnemonic,ptr->mnemonic[0],NULL);
	}
	if ( ptr->accelerator ) {
	    XtVaSetValues(cascade,XmNaccelerator,ptr->accelerator,NULL);
	}
    } else { /* XG_MENU_NONE */
        /* create MENU_NONE item...whatever it is */
	if ( ptr->class == &xmSeparatorWidgetClass ) {
	    cascade = XtVaCreateManagedWidget(name,
		*ptr->class, parent,
		NULL);
	} else if ( ptr->class == &xmToggleButtonWidgetClass ) {
            xmstring = XmStringCreateSimple("Disable");
            cascade = XtVaCreateManagedWidget(name,
                *ptr->class, parent,
                XmNlabelString, xmstring,
                XmNvisibleWhenOff, True,
                XmNset, True,
                NULL);
            XmStringFree(xmstring);
	} else {
            xmstring = XmStringCreateSimple(name);
            cascade = XtVaCreateManagedWidget(name,
                *ptr->class, parent,
                XmNlabelString, xmstring,
                NULL);
            XmStringFree(xmstring);
	}
    }

    /* now add the callback (if any) or subitems (unless there are none) */
    if ( type == XG_MENU_NONE || type == XG_MENU_HACK ) {
	if ( ptr->class != &xmSeparatorWidgetClass ) {
	    if ( ptr->mnemonic ) {
	        XtVaSetValues(cascade,XmNmnemonic,ptr->mnemonic[0],NULL);
	    }
	    if ( ptr->accelerator ) {
	        XtVaSetValues(cascade,XmNaccelerator,ptr->accelerator,NULL);
	    }
	    if ( ptr->callback ) {
		XtAddCallback(cascade,
		    (ptr->class == &xmToggleButtonWidgetClass) ?
			 XmNvalueChangedCallback : XmNactivateCallback,
		    ptr->callback, ptr->callback_data);
	    }
	}
    } else if ( type == XG_MENU_TEAR_OFF_H || type == XG_MENU_TEAR_OFF_V ) {
	char *menuName;
	Widget menubar;
	Widget shell, menurc, form;
        Atom protocol;

	menuName = XtMalloc(strlen(name) + 1);
	strcpy(menuName,name);
        shell = XtVaCreatePopupShell(menuName, vendorShellWidgetClass,
	    _XG_Global.applShell, 
	    XmNinput,True, 
	    XmNallowShellResize,True, 
            XmNwindowGroup, XtWindow(_XG_Global.applShell),
	    XmNtransient, True,
	    NULL);

        protocol = XmInternAtom(XtDisplay(shell), "WM_DELETE_WINDOW", False);
        XmAddWMProtocols(shell, &protocol, 1);
        XtAddEventHandler(shell, NoEventMask, True, _XgWMClientMessage, shell);

        if ( XmIsMotifWMRunning(shell) ) {
            unsigned int decor_flags, func_flags;

            decor_flags = MWM_DECOR_BORDER | MWM_DECOR_RESIZEH;
            decor_flags |= MWM_DECOR_TITLE | MWM_DECOR_MENU;

            func_flags = MWM_FUNC_CLOSE | MWM_FUNC_RESIZE;
            func_flags |= MWM_FUNC_MOVE;

            XtVaSetValues(shell,
                XmNmwmDecorations, decor_flags,
                XmNmwmFunctions, func_flags,
                NULL);
        }

	if ( type == XG_MENU_TEAR_OFF_V ) {
	    form = XtVaCreateManagedWidget("tear_off_form", xmFormWidgetClass,
		shell, NULL);
	    menurc = XtVaCreateManagedWidget("tear_off_rc", 
		xmRowColumnWidgetClass, form, XmNpacking, XmPACK_TIGHT, 
		XmNorientation, XmVERTICAL, XmNcolumns, 1, 
		XmNtopAttachment, XmATTACH_FORM, 
		XmNleftAttachment, XmATTACH_FORM, 
		XmNrightAttachment, XmATTACH_FORM,
		XmNbottomAttachment, XmATTACH_FORM, NULL);
	} else {
	    menubar = XmCreateMenuBar(shell,"MenuBar",NULL,0);
	}

	for ( i = 0; i < ptr->subitem_count; i++ ) {
	    Widget widget;

	    if ( ptr->subitems[i].submenu_type == XG_MENU_PULLRIGHT ) {

		if ( type == XG_MENU_TEAR_OFF_V ) {
		    menubar = XmCreateMenuBar(menurc,"MenuBar",NULL,0);
		}
		widget = BuildItem(menubar, ptr->subitems[i].submenu_type,
		    ptr->subitems[i].label,
		    ptr->subitems[i].mnemonic,
		    ptr->subitems[i].accelerator,
		    &ptr->subitems[i]);

		XtManageChild(menubar);
	    } else {
		Widget thisParent = 
		    (type == XG_MENU_TEAR_OFF_V) ? menurc : menubar;
		int thisType = 
		    (type == XG_MENU_TEAR_OFF_V) ? 
		    ptr->subitems[i].submenu_type : XG_MENU_HACK;

		widget = BuildItem(thisParent, thisType,
		    ptr->subitems[i].label,
		    ptr->subitems[i].mnemonic,
		    ptr->subitems[i].accelerator,
		    &ptr->subitems[i]);
	    }
	}

	XtAddCallback(cascade, XmNactivateCallback, PopupTearOff, shell);
	
    } else {
	for ( i = 0; i < ptr->subitem_count; i++ ) {
	    Widget widget;
	    widget = BuildItem(menu, ptr->subitems[i].submenu_type,
	        ptr->subitems[i].label,
	        ptr->subitems[i].mnemonic,
	        ptr->subitems[i].accelerator,
	        &ptr->subitems[i]);
	}
    }

    return cascade;
}
