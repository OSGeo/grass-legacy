#include    "map.h"
#include    <stdio.h>
#include "digit.h"

static Widget digpop, button_area;
static int dig_action;

take_dig_action (w, action)
    Widget w;
    int action;
{
    dig_action = action;
}

make_dig_popup (parent)
    Widget parent;
{
    Widget rc, message, digform;
    int i, n, x, y;
    Arg wargs[10];
    XmString str;
    Widget make_button_area2();
    n = 0;
   
   n = 0;
   
   digpop = XtCreatePopupShell ("Register",transientShellWidgetClass, 
					     parent, NULL, 0); 
   XtVaSetValues (digpop, XmNsaveUnder, True, XtNy, Winy, XtNx, Winx, NULL);

    digform = XtVaCreateManagedWidget ("digform", xmFormWidgetClass, digpop,  
				      XmNwidth, 250, XmNheight, 150,
				      NULL);
    str = 
    XmStringCreateSimple ("Use digitizer keys or buttons below for input\n");
    message = XtVaCreateManagedWidget ("message", xmLabelGadgetClass, digform, 
		   XmNtopAttachment, XmATTACH_FORM,
		   XmNleftAttachment, XmATTACH_FORM,
		   XmNrightAttachment, XmATTACH_FORM,
		   XmNlabelString, str, NULL);
    XtFree (str); 
    
    button_area = make_button_area2(digform, message);
}

Widget
make_button_area2(parent, top)
    Widget parent, top;
{
    Widget rc, buttons[4];
    char buf[50];
    int i;
    XmString str[4];
    int first;
    int take_dig_action();

    first = D_start_button();
   
    sprintf (buf, "%d -- digitize  point", first);
    str[0] = XmStringCreateSimple (buf);
    sprintf (buf, "%d -- quit digitizing", first + 1);
    str[1] = XmStringCreateSimple (buf);
    sprintf (buf, "%d -- update_monitor", first + 2);
    str[2] = XmStringCreateSimple (buf); 
    sprintf (buf, "%d -- toggle point/stream mode", first + 3);
    str[3] = XmStringCreateSimple (buf);

    rc = XtVaCreateManagedWidget ("rc", xmRowColumnWidgetClass, parent, 
		   XmNtopAttachment, XmATTACH_WIDGET,
		   XmNtopWidget, top,
		   XmNbottomAttachment, XmATTACH_FORM,
		   XmNleftAttachment, XmATTACH_FORM,
		   XmNrightAttachment, XmATTACH_FORM,
			       NULL);
    for (i = 0; i < 4; i ++)
    {
	sprintf (buf, "%d", i); 
        buttons[i] = XtVaCreateManagedWidget (buf, xmPushButtonGadgetClass, rc,
	           XmNlabelString , str[i],
                        NULL);
    }
    XtAddCallback (buttons[0], XmNactivateCallback, take_dig_action, 1);
    XtAddCallback (buttons[1], XmNactivateCallback, take_dig_action, 2);
    XtAddCallback (buttons[2], XmNactivateCallback, take_dig_action, 3);
    XtAddCallback (buttons[3], XmNactivateCallback, take_dig_action, 4);
    
    return rc;
}
 


show_dig_menu()
{
    extern Widget toplevel;
    
    XtManageChild (digpop);
    XmUpdateDisplay (toplevel);
}

close_dig_menu()
{
    XtUnmanageChild (digpop);
}

xdig_readall( x, y)
	int  *x, *y ;
{
    extern Widget monolog;
    char  key ;
    XEvent event;
    Window win1;


    win1 = XtWindow (button_area);

    dig_action = 0;
    
    XmUpdateDisplay (toplevel);
    dig_action =  D_readall( x, y) ;
    while (XCheckMaskEvent (dpy,
            ButtonPressMask | ButtonReleaseMask |
            KeyPressMask | KeyReleaseMask, &event))
    {
        if (event.xany.window == win1)
          XtDispatchEvent (&event);
        else
            XBell (dpy,50);
	XFlush (dpy);
        XmUpdateDisplay (toplevel);
     }

    return( dig_action) ;
}
