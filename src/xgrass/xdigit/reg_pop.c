#include "digit.h"
#include    "map.h"
#include    <stdio.h>

static Widget coors[MAX_COOR + 1][3], labels[MAX_COOR + 1];
static Widget regpop, button_area, regform;
static int reg_action;
static Pixel fg, bg;

take_reg_action (w, action)
    Widget w;
    int action;
{
    reg_action = action;
}

make_reg_popup (parent)
    Widget parent;
{
    Widget rc2, rc, message, info;
    int i, n, x, y, ncols;
    Arg wargs[10];
    XmString t, t2;
    
    n = 0;
   t = XmStringCreateSimple (" ");

   n = 0;
   XtSetArg (wargs[n], XtNx, Winx + Wdth/4); n++;
   XtSetArg (wargs[n], XtNy, Winy + Hght/8); n++;
   XtSetArg (wargs[n],  XmNselectionLabelString,t); n++;

   regpop = XmCreatePromptDialog (toplevel, "Register",wargs, n);

    regform = XtVaCreateManagedWidget ("regform", xmFormWidgetClass, regpop,  
				     NULL);
    
    rc = XtVaCreateManagedWidget ("rc", xmRowColumnWidgetClass, regform, 
		   XmNtopAttachment, XmATTACH_POSITION,
		   XmNtopPosition, 5,
		   XmNleftAttachment, XmATTACH_FORM,
		   XmNentryAlignment, XmALIGNMENT_END,
		   XmNisAligned, TRUE,
			       NULL);
    rc2 = XtVaCreateManagedWidget ("rc2", xmRowColumnWidgetClass, regform, 
		   XmNtopAttachment, XmATTACH_POSITION,
		   XmNtopPosition, 5,
		   XmNleftAttachment, XmATTACH_WIDGET,
		   XmNleftWidget, rc, 
		   XmNrightAttachment, XmATTACH_FORM,
		   XmNorientation, XmHORIZONTAL,
		   XmNnumColumns, MAX_COOR + 1,
		   XmNentryAlignment, XmALIGNMENT_BEGINNING,
		   XmNpacking, XmPACK_COLUMN,
			       NULL);
    t = XmStringCreateSimple("REGISTER POINTS");
    message = XtVaCreateManagedWidget ("", xmLabelWidgetClass, regform, 
	           XmNlabelString ,t,
		   XmNtopAttachment, XmATTACH_FORM,
		   XmNleftAttachment, XmATTACH_FORM,
		   XmNrightAttachment, XmATTACH_FORM,
		   XmNbottomAttachment, XmATTACH_WIDGET,
		   XmNbottomWidget, rc2,
                        NULL);
    XtFree (t);

    t = XmStringCreateSimple("Use buttons below or digitizer buttons for input");
    info = XtVaCreateManagedWidget ("", xmLabelWidgetClass, regform, 
	           XmNlabelString ,t,
		   XmNtopAttachment, XmATTACH_WIDGET,
		   XmNtopWidget, rc,
		   XmNleftAttachment, XmATTACH_FORM,
		   XmNrightAttachment, XmATTACH_FORM,
                        NULL);
    XtFree (t);

    button_area = make_button_area (regform, info);

    XtVaGetValues (info, XtNforeground, &fg, XtNbackground, &bg, NULL);
    ncols = 20;
    for (i = 0; i < MAX_COOR + 1; i ++)
    {
        labels[i] = XtVaCreateManagedWidget ("", xmLabelWidgetClass, rc ,
		      XmNcolumns, 5,
                      XmNmarginTop , 5,
                      XmNmarginBottom , 7,
                        NULL);
        coors[i][0] = XtVaCreateManagedWidget ("", xmLabelWidgetClass, rc2, 
		      XmNcolumns, ncols,
                      XmNmarginTop , 5,
                      XmNmarginBottom , 7,
		      XmNborderColor, fg,
		      XmNborderWidth, 1,
                        NULL);
        coors[i][1] = XtVaCreateManagedWidget ("", xmLabelWidgetClass, rc2, 
		      XmNcolumns, ncols,
                      XmNmarginTop , 5,
                      XmNmarginBottom , 7,
		      XmNborderColor, fg,
		      XmNborderWidth, 1,
                        NULL);
        coors[i][2] = XtVaCreateManagedWidget ("", xmLabelWidgetClass, rc2, 
		      XmNcolumns, ncols,
                      XmNmarginTop , 5,
                      XmNmarginBottom , 7,
		      XmNborderColor, fg,
		      XmNborderWidth, 1,
                        NULL);
    }
    t = XmStringCreateSimple( "Point");
    XtVaSetValues (labels[0], XmNlabelString, t, NULL);
    XtFree (t);
#ifdef LATLON
    if (ll_flag)
    {
        t = XmStringCreateSimple( "Latitude");
        t2 = XmStringCreateSimple( "Longitude");
    }
    else 
#endif
    {
        t = XmStringCreateSimple( "X Coordinate");
        t2 = XmStringCreateSimple( "Y Coordinate");
    }
    XtVaSetValues (coors[0][0], 
		XmNlabelString, t, 
		XmNborderWidth, 0, NULL);
    XtVaSetValues (coors[0][1], 
                XmNlabelString, t2, 
		XmNborderWidth, 0, NULL);
    XtFree (t);
    XtFree (t2);
    t = XmStringCreateSimple( "Residuals");
    XtVaSetValues (coors[0][2], XmNlabelString, t, 
		XmNborderWidth, 0, NULL);
    XtFree (t);
    
    
    XtUnmanageChild (XmSelectionBoxGetChild(regpop, XmDIALOG_HELP_BUTTON));
    XtUnmanageChild (XmSelectionBoxGetChild(regpop, XmDIALOG_COMMAND_TEXT));
    XtUnmanageChild (XmSelectionBoxGetChild(regpop, XmDIALOG_CANCEL_BUTTON));
    XtUnmanageChild (XmSelectionBoxGetChild(regpop, XmDIALOG_OK_BUTTON));

#ifdef LATLON
 if (ll_flag)
    set_reg_coors_ll(0);
 else
#endif
    set_reg_coors(0);

}

Widget
make_button_area (parent, top)
    Widget parent, top;
{
    Widget rc, buttons[5];
    char buf[50];
    int i;
    XmString str[5];
    int first;
    int take_reg_action();

    first = D_start_button();
   
    sprintf (buf, "%d -- register point", first);
    str[0] = XmStringCreateSimple (buf);
    sprintf (buf, "%d -- skip  point", first + 1);
    str[1] = XmStringCreateSimple (buf);
    sprintf (buf, "%d -- unregister point", first + 2);
    str[2] = XmStringCreateSimple (buf); 
    sprintf (buf, "%d -- add more points", first + 3);
    str[3] = XmStringCreateSimple (buf);
    sprintf (buf, "%d -- accept residuals", first + 4);
    str[4] = XmStringCreateSimple (buf);

    rc = XtVaCreateManagedWidget ("rc", xmRowColumnWidgetClass, parent, 
		   XmNtopAttachment, XmATTACH_WIDGET,
		   XmNtopWidget, top,
		   XmNbottomAttachment, XmATTACH_FORM,
		   XmNleftAttachment, XmATTACH_FORM,
		   XmNrightAttachment, XmATTACH_FORM,
		   XmNnumColumns, 1,
		   XmNpacking, XmPACK_COLUMN,
			       NULL);
    for (i = 0; i < 5; i ++)
    {
	sprintf (buf, "%d", i); 
        buttons[i] = XtVaCreateManagedWidget (buf, xmPushButtonGadgetClass, rc,
	           XmNlabelString , str[i],
                        NULL);
    }
    XtAddCallback (buttons[0], XmNactivateCallback, take_reg_action, 1);
    XtAddCallback (buttons[1], XmNactivateCallback, take_reg_action, 2);
    XtAddCallback (buttons[2], XmNactivateCallback, take_reg_action, 3);
    XtAddCallback (buttons[3], XmNactivateCallback, take_reg_action, 4);
    XtAddCallback (buttons[4], XmNactivateCallback, take_reg_action, 5);
    
    return rc;
}
 

set_active (active)
    int active;
{
    static int last = 0;
    int i;
    char s[10];
    
    XmString t = XmStringCreateSimple("*"), num;
    XmString curr = XmStringCreateSimple("->");
    
 /*   XtVaSetValues (labels[last], XmNbackground, bg, XmNforeground, 
						fg, NULL);*/
    last = active;
    for (i = 0; i < MAX_COOR; i++)
    {
	if (use[i])
	    XtVaSetValues (labels[i+1], XmNlabelString ,t, NULL);
        else
	{
	    sprintf (s, "%d", i+1);
	    num = XmStringCreateSimple(s);
	    XtVaSetValues (labels[i+1], XmNlabelString ,num, NULL);
	    XtFree (num);
	}
    }
    XtVaSetValues (labels[active + 1], XmNlabelString ,curr, NULL);
   XtFree (curr);
}
   
set_reg_coors(active)
    int active;
{
    int i;
    char buf[100], s[10];
    XmString t,t1;

#ifdef LATLON
/*
    if (ll_flag) {
    t1 = XmStringCreateSimple(
	 "Point     Latitude       Longtitude      Residuals"); 
    XtVaSetValues(regform, XmNlabelString, t1, NULL);
*/


    for (i = 0; i < MAX_COOR; i++)
    {
	if (bcx[i][0] !='\0' || bcy[i][0]!= '\0')
	{
            sprintf (buf, "%s", bcx[i]);
	    t = XmStringCreateSimple (buf);
	    XtVaSetValues(coors[i+1][0], XmNlabelString, t, NULL);
   	    XtFree (t);
            
	    sprintf (buf, "%s", bcy[i]);
	    t = XmStringCreateSimple (buf);
	    XtVaSetValues(coors[i+1][1], XmNlabelString, t, NULL);
   	    XtFree (t);
	    
	}
    }
    /*
  }
    else 
    */
#endif
    
    for (i = 0; i < MAX_COOR; i++)
    {
	if (bx[i] || by[i])
	{
            sprintf (buf, "%9lf", bx[i]);
	    t = XmStringCreateSimple (buf);
	    XtVaSetValues(coors[i+1][0], XmNlabelString, t, NULL);
   	    XtFree (t);
            
	    sprintf (buf, "%9lf", by[i]);
	    t = XmStringCreateSimple (buf);
	    XtVaSetValues(coors[i+1][1], XmNlabelString, t, NULL);
   	    XtFree (t);
	    
	}
    }
    set_active (active);
}

set_reg_coors_ll(active)
    int active;
{
    int i;
    char buf[100], s[10];
    XmString t,t1;
/*
    t1 = XmStringCreateSimple(
	 "Point     Latitude       Longtitude      Residuals"); 
    XtVaSetValues(regform, XmNlabelString, t1, NULL);

*/


    for (i = 0; i < MAX_COOR; i++)
    {
	if (bcx[i][0] !='\0' || bcy[i][0]!= '\0')
	{
            sprintf (buf, "%s", bcx[i]);
	    t = XmStringCreateSimple (buf);
	    XtVaSetValues(coors[i+1][0], XmNlabelString, t, NULL);
   	    XtFree (t);
            
	    sprintf (buf, "%s", bcy[i]);
	    t = XmStringCreateSimple (buf);
	    XtVaSetValues(coors[i+1][1], XmNlabelString, t, NULL);
   	    XtFree (t);
	    
	}
    }
    set_active (active);
}

show_reg_ask()
{
    extern Widget toplevel;
    
#ifdef LATLON
 if (ll_flag)
    set_reg_coors_ll(0);
 else
#endif
    set_reg_coors(0);
    XtManageChild (regpop);
    XmUpdateDisplay (toplevel);
}

show_residual_results (n_points)
    int n_points;
{
    int i;
    char buf[20];
    XmString t;

    for (  i = 0 ;  i < n_points;  i++ )
    {
        sprintf (buf, "%9lf", residuals[i]);
	t = XmStringCreateSimple (buf);
	XtVaSetValues(coors[i+1][2], XmNlabelString, t, NULL);
        XtFree (t);
    }
    calculate_map_scale ();
}
close_reg_ask()
{
    XtUnmanageChild (regpop);
}

get_reg_response( x, y)
	double  *x, *y ;
{
    extern Widget monolog;
    int  xraw, yraw ;
    char  key ;
    XEvent event;
    Window win1, win2;


    win1 = XtWindow (button_area);
    win2 = XtWindow (monolog);
    while( 1)
    {
        XmUpdateDisplay (toplevel);
	reg_action =  ask_all( x, y) ;
        while (XCheckMaskEvent (dpy,
            ButtonPressMask | ButtonReleaseMask |
            KeyPressMask | KeyReleaseMask, &event))
        {
        if ((event.xany.window == win1)||(event.xany.window == win2))
          XtDispatchEvent (&event);
        else
            XBell (dpy,50);
	XFlush (dpy);
        }

	if (reg_action >= 1  && reg_action  <= 5)
		break ;
    }
    return( reg_action) ;
}
