/*  @(#)reset_map.c	2.1  6/26/87  */

/*
*  This also sets up the map the first time.
*/
#include "digit.h"
#include	<stdio.h>

static Widget checkpop, coordinfo;
static int button;

button_hit(w, n)
    Widget w;
    int n;
{
    button = n;
}

check_map( Map)
	struct Map_info *Map;
{
	int ok ;
	int priority_on ;

	check_scale (Map) ;


	priority_on = set_priority() ;

	check_map_buttons() ;

	if ( priority_on == 0)
	    unset_priority() ;
	
	return(0) ;
}

Widget
make_explain(parent) 
    Widget parent;
{
    char buf[100];
    XmString str[4];
    Widget rc, label[4];
    int first_button ;
    int i;
    
      
    first_button = D_start_button() ;
    rc = XtVaCreateManagedWidget ("explain", xmRowColumnWidgetClass, parent,
		       XmNentryAlignment, XmALIGNMENT_CENTER,
			XmNleftAttachment, XmATTACH_FORM,
			XmNrightAttachment, XmATTACH_FORM,
						  NULL);
    
    
    str[0]  = XmStringCreateSimple (
		"Verify that the map registration is correct.") ;
    str[1]  = XmStringCreateSimple (
	"Check known points on the map with the digitizer") ;
    str[2]  = XmStringCreateSimple (
	" and compare them with the coordinates below.") ;
    sprintf(buf, " Key '%d' to preserve point;  Any other number to continue", 
						    first_button) ;
    str[3]  = XmStringCreateSimple (buf);
   
    for (i = 0; i < 4; i++)
    {
        label [i] = XtVaCreateManagedWidget ("", xmLabelWidgetClass, rc, 
			XmNlabelString, str[i],
                        NULL);
        XtFree (str[i]);
    }
    return rc;
}

update_coords (x1, y1, x2, y2)
  double x1, y1, x2, y2;
{
    char message[128] ;
    extern Widget toplevel;
    static double ox1, ox2, oy1, oy2;
    
    if ((x1 != ox1) || (x2 != ox2) || (y1 != oy1) || (y2 != oy2))
    {

        sprintf(message, "      %12.2lf %12.2lf   %12.2lf %12.2lf", x1, y1, x2, y2);
	XmTextSetString (coordinfo, message);

        XmUpdateDisplay (toplevel);
        ox1 = x1; oy1 = y1;
        ox2 = x2; oy2 = y2;
    
    }
}
check_map_buttons ()
{
    double   Xmapcoor;
    double   Ymapcoor;
    double   Xsaved ;
    double   Ysaved ;
    XEvent event;
    Window win;
    int Button;

    Xsaved = 0.0 ;
    Ysaved = 0.0 ;
            XFlush (dpy);

    XtManageChild (checkpop);
    XmUpdateDisplay (toplevel);
    XFlush (dpy);
    win = XtWindow (checkpop);
    button = Button = 0;
    update_coords (0.0, 0.0, 0.0, 0.0);
    for(;;)
    {
            XFlush (dpy);
                
	    while (XCheckMaskEvent (dpy,
                ButtonPressMask | ButtonReleaseMask |
                KeyPressMask | KeyReleaseMask, &event))
            {
            if (event.xany.window == win)
                XtDispatchEvent (&event);
            else
                XBell (dpy,50);
            XFlush (dpy);
            }
	    Button = _coll_a_pnt ( &Xmapcoor, &Ymapcoor) ;
	    update_coords (Xmapcoor, Ymapcoor, Xsaved, Ysaved);

	    if (Button || button)
	    {
		if (Button == 1 || button == 1 )
		{
		    Xsaved = Xmapcoor ;
		    Ysaved = Ymapcoor ;
		}
		else
		    break ;
	    }
	}
	XtUnmanageChild (checkpop);

}  /*  check_map_buttons()  */



make_check_popup (parent)
    Widget parent;
{
    Widget checkform, rc, sep, coordlabel;
    int i, n;
    Arg wargs[20];
    XmString save = XmStringCreateSimple("save");
    XmString done = XmStringCreateSimple("done");
    XmString title = XmStringCreateSimple("Check Map");
    XmString t = XmStringCreateSimple(" ");

    checkpop = XmCreatePromptDialog (toplevel, "Check Map", NULL, 0);
    XtVaSetValues (checkpop, XmNautoUnmanage, False,
    	    XmNapplyLabelString, save,
	    XmNcancelLabelString, done,
	    XmNselectionLabelString,t, NULL);
    XtVaSetValues (XtParent(checkpop), 
		XmNsaveUnder, True,
		NULL);
    XtFree (t);

    
    checkform = XtVaCreateManagedWidget("checkform", xmFormWidgetClass, 
		checkpop, NULL);
    rc = make_explain (checkform);
    
    sep = XtVaCreateManagedWidget ("sep", xmSeparatorGadgetClass, checkform,
			XmNleftAttachment, XmATTACH_FORM,
			XmNrightAttachment, XmATTACH_FORM,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, rc,
			NULL);
    
    t  = XmStringCreateSimple (
	     "    X -  Current  - Y                 X -  Saved  - Y ") ;
    coordlabel = XtVaCreateManagedWidget ("", xmLabelWidgetClass, checkform, 
			XmNleftAttachment, XmATTACH_FORM,
			XmNrightAttachment, XmATTACH_FORM,
			XmNlabelString, t,
			XmNtopAttachment, XmATTACH_WIDGET,
			XmNtopWidget, sep,
			NULL);
    n = 0;
    XtSetArg (wargs[n], XmNbottomAttachment, XmATTACH_FORM); n++;
    XtSetArg (wargs[n], XmNtopAttachment, XmATTACH_WIDGET); n++;
    XtSetArg (wargs[n], XmNtopWidget, coordlabel); n++;
    XtSetArg (wargs[n], XmNeditable, FALSE); n++;
    XtSetArg (wargs[n], XmNcursorPositionVisible, FALSE); n++;
    XtSetArg (wargs[n], XmNrows, 1); n++;
    XtSetArg (wargs[n], XmNcolumns, 50); n++;
    XtSetArg (wargs[n], XmNshadowThickness, 0); n++;

    coordinfo = XmCreateText (checkform, "coordinfo", wargs, n);
    XtManageChild (coordinfo);
 
    XtFree (t);
    XtFree (done);
    XtFree (save);
    XtFree (title);
    XtAddCallback(checkpop, XmNapplyCallback, button_hit, 1);
    XtAddCallback(checkpop, XmNcancelCallback, button_hit, 2);

    XtManageChild (XmSelectionBoxGetChild(checkpop, XmDIALOG_APPLY_BUTTON));
    XtUnmanageChild (XmSelectionBoxGetChild(checkpop, XmDIALOG_OK_BUTTON));
    XtUnmanageChild (XmSelectionBoxGetChild(checkpop, XmDIALOG_HELP_BUTTON));
    XtUnmanageChild (XmSelectionBoxGetChild(checkpop, XmDIALOG_COMMAND_TEXT));


}
