/*
*  Functions to ask user questions and if possible use the digitizer cursor for
*  input.
*    ask_yes_no(),  ask_driver_yes_no(),  ask_driver()
*
*	GRASS 3.0, Spring 88,    -mh
*/

/*
*  Before using ask_driver_yes_no() make a call to D_cursor_buttons() to make
*  sure that a specific driver can handle the question.  Otherwise use the
*  keyboard for input.
*/
#include "digit.h"


static int button;

yes_no_hit(w, n)
    Widget w;
    int n;
{
    button = n;
}

Widget
make_yn_popup (parent, header)
    Widget parent;
    char *header;
{
    Widget checkform;
    int i, n, first;
    char buf[100];
    Arg wargs[10];
    XmString yes= XmStringCreateSimple("yes");
    XmString no= XmStringCreateSimple("no");
    XmString title = XmStringCreateSimple("Digitize");
    XmString message;
    Widget yesnopop;
 
    first = D_start_button();
    sprintf (buf, "<%d> for yes, <%d> for no. (Or use buttons)",
		    first, first+1); 
    message = XmStringCreateSimple(buf);
    yesnopop = XmCreatePromptDialog (toplevel, "Yes/No", NULL, 0);
    XtVaSetValues (yesnopop, XmNokLabelString, yes,
			       XmNcancelLabelString, no,
			       XmNselectionLabelString, message, NULL);
    XtFree (message);

    
    checkform = XtVaCreateManagedWidget("checkform", xmFormWidgetClass, 
		yesnopop, NULL);
    
    
    message  = XmStringCreateSimple (header);
    XtVaCreateManagedWidget ("", xmLabelWidgetClass, checkform, 
			XmNlabelString, message,
			XmNtopAttachment, XmATTACH_FORM,
			NULL);
    
    XtFree (message);
    XtFree (no);
    XtFree (yes);
    XtFree (title);
    XtAddCallback(yesnopop, XmNokCallback, yes_no_hit, 1);
    XtAddCallback(yesnopop, XmNcancelCallback, yes_no_hit, 2);

    XtUnmanageChild (XmSelectionBoxGetChild(yesnopop, XmDIALOG_HELP_BUTTON));
    XtUnmanageChild (XmSelectionBoxGetChild(yesnopop, XmDIALOG_COMMAND_TEXT));
    XtManageChild (yesnopop);

    return (yesnopop);

}

ask_driver_yes_no(quest)
	char  *quest ;
{
    XEvent event;
    Window win;
    double x, y;
    int  priority_on ;
    extern Widget toplevel;
    Widget yesnopop;


    yesnopop = make_yn_popup(toplevel, quest);
    win = XtWindow (yesnopop);
    XFlush (dpy);
    XmUpdateDisplay (toplevel);
    
    button = 0;    
    priority_on = set_priority() ;

/*DEBUG*/ debugf ("ask_driver_yes_no\n");
    for(;;)
    {
        XmUpdateDisplay (toplevel);
        button = ask_all(&x, &y) ;
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


	    if (button)
	        break ;
	}
    	if (priority_on == 0)
		unset_priority() ;
	XtUnmanageChild (yesnopop);
	return ((button == 1)? 1:0);

}  /*  ask_driver_yes_no  */

/*  ask until any key is hit, returns which button hit  */
ask_driver()
{
	double  X ;
	double  Y ;

	return( ask_driver_raw( &X, &Y) ) ;
}

get_digitizer_button_xy (X, Y)
	double  *X ;
	double  *Y ;
{
	double Xraw, Yraw;
	int ret;

	ret = ask_driver_raw( &Xraw, &Yraw) ;
	transform_a_into_b ((double)Xraw, (double)Yraw, X, Y) ;

	return ret;
}


/*  making calls directly to driver  */

ask_driver_raw( X, Y) 
	double  *X ;
	double  *Y ;
{
	int  button ;
	int  priority_on ;

	priority_on = set_priority() ;
	button = D_ask_driver_raw( X, Y) ;

    	if (priority_on == 0)
		unset_priority() ;

	return(button) ;
}

ask_all( X, Y) 
	double  *X ;
	double  *Y ;
{
	int  button ;
	int  priority_on ;
	int  x, y;

	priority_on = set_priority() ;
	button = get_diginput( &x, &y) ;

    	if (priority_on == 0)
		unset_priority() ;
	*X = (double)x;
	*Y = (double)y;

	return(button) ;
}
coll_a_pnt ( x, y)
    double    *x;
    double    *y;
{


    D_clear_driver() ;
    
    return( _coll_a_pnt(x,y) ) ;

}	    /*  coll_a_pnt ()  */


_coll_a_pnt ( x, y)
    double    *x;
    double    *y;
{

    int	Xraw, Yraw;
    int	KeyHit ;

    KeyHit = D_read_raw (&Xraw, &Yraw) ;
    
    transform_a_into_b ((double) Xraw, (double) Yraw, x, y) ;
#ifdef DEBUG
    debugf ("BUTTON: %d RAW: x = %d y = %d    WORLD: x = %lf y = %lf\n",
				   KeyHit, Xraw, Yraw, *x, *y);
#endif

    return(KeyHit ) ;

}	    /*  _ coll_a_pnt ()  */
