#include "digit.h"

#include    "map.h"
#include "gis.h"
#include    <stdio.h>

static Widget xlabel,ylabel,rc,rc2,point;

char	*star = "*" ;
static Widget coors[MAX_COOR][2], labels[MAX_COOR], coorpop, message, coorform;
static coor_action = 0;

action_taken (w, action)
    Widget w;
    int action;
{
    coor_action = action;
}

make_coor_popup (parent)
    Widget parent;
{
    Widget label;
    int i, n, x, y;
    XmString t;
    char buf[100];
    int first;

    first = D_start_button();
   
    t = XmStringCreateSimple (" ");
#ifdef LATLON
  if (ll_flag)
    coorpop = XmCreatePromptDialog (toplevel, "Enter coordinates in Lat/Lon", NULL, 0);
  else
#endif
    coorpop = XmCreatePromptDialog (toplevel, "Enter coordinates", NULL, 0);
    XtVaSetValues (coorpop, XmNautoUnmanage, TRUE,
    	    XmNselectionLabelString,t, NULL);
    XtFree (t);

    coorform = XtVaCreateManagedWidget ("coorform", xmFormWidgetClass, coorpop, 
		   XmNancestorSensitive, True,
		   NULL);
    
    rc = XtVaCreateManagedWidget ("rc", xmRowColumnWidgetClass, coorform, 
		   XmNtopAttachment, XmATTACH_POSITION,
		   XmNtopPosition, 10,
		   XmNleftAttachment, XmATTACH_FORM,
		   XmNentryAlignment, XmALIGNMENT_END,
		   XmNisAligned, TRUE,
			       NULL);
    rc2 = XtVaCreateManagedWidget ("rc2", xmRowColumnWidgetClass, coorform, 
		   XmNtopAttachment, XmATTACH_POSITION,
		   XmNtopPosition, 10,
		   XmNleftAttachment, XmATTACH_WIDGET,
		   XmNleftWidget, rc, 
		   XmNrightAttachment, XmATTACH_FORM,
		   XmNorientation, XmHORIZONTAL,
		   XmNnumColumns, MAX_COOR,
		   XmNpacking, XmPACK_COLUMN,
     	           XmNentryAlignment, XmALIGNMENT_BEGINNING,

			       NULL);
    sprintf (buf, "Use digitizer <%d> for ok <%d> for cancel", first, first +1);
    t = XmStringCreateSimple(buf);
    label  = XtVaCreateManagedWidget ("label", xmLabelGadgetClass, coorform, 
		   XmNtopAttachment, XmATTACH_WIDGET,
		   XmNtopWidget, rc,
		   XmNbottomAttachment, XmATTACH_FORM,
	           XmNlabelString ,t,
                   NULL);
    XtFree (t);
    t = XmStringCreateSimple("");
    point = XtVaCreateManagedWidget ("point", xmLabelGadgetClass, coorform, 
	           XmNlabelString ,t,
		   XmNnumColumns, 10,
		   XmNentryAlignment, XmALIGNMENT_BEGINNING,
		   XmNleftAttachment, XmATTACH_FORM,
		   XmNbottomAttachment, XmATTACH_WIDGET,
		   XmNbottomWidget, rc,
                        NULL);
    XtFree (t);

#ifdef LATLON 
 if (ll_flag)
    t = XmStringCreateSimple("Latitude");
 else
#endif
    t = XmStringCreateSimple("X Coordinate");
    xlabel = XtVaCreateManagedWidget ("xlabel", xmLabelGadgetClass, coorform, 
	           XmNlabelString ,t,
		   XmNnumColumns, 20,
		   XmNentryAlignment, XmALIGNMENT_END,
		   XmNleftAttachment, XmATTACH_WIDGET,
		   XmNleftWidget, point,
		   XmNbottomAttachment, XmATTACH_WIDGET,
		   XmNbottomWidget, rc2,
                        NULL);
    XtFree (t);

#ifdef LATLON
 if (ll_flag)
    t = XmStringCreateSimple("Longtitude");
 else
#endif
    t = XmStringCreateSimple("Y Coordinate");
    ylabel = XtVaCreateManagedWidget ("ylabel", xmLabelGadgetClass, coorform, 
	           XmNlabelString ,t,
		   XmNnumColumns, 20,
		   XmNentryAlignment, XmALIGNMENT_CENTER,
		   XmNrightAttachment, XmATTACH_FORM,
		   XmNbottomAttachment, XmATTACH_WIDGET,
		   XmNbottomWidget, rc2,
                        NULL);
    XtFree (t);
    message = XtVaCreateManagedWidget ("", xmLabelGadgetClass, coorform, 
		   XmNtopAttachment, XmATTACH_FORM,
		   XmNleftAttachment, XmATTACH_FORM,
		   XmNrightAttachment, XmATTACH_FORM,
		   XmNbottomAttachment, XmATTACH_WIDGET,
		   XmNbottomWidget, point,
		   XmNancestorSensitive, True,
                        NULL);

#ifdef LATLON
  if (ll_flag) {
    for (i = 0; i < MAX_COOR; i ++)
    {
        labels[i] = XtVaCreateManagedWidget ("", xmLabelGadgetClass, rc ,
		      XmNcolumns, 2,
                      XmNmarginTop , 7,
                      XmNmarginBottom , 7,
                        NULL); 
        coors[i][0] = XtVaCreateManagedWidget ("", xmTextFieldWidgetClass, rc2, 
		      XmNmarginLeft , 5,
		      XmNmaxLength, 20,
                        NULL);
        coors[i][1] = XtVaCreateManagedWidget ("", xmTextFieldWidgetClass, rc2, 
		      XmNmarginLeft , 5,
		      XmNmaxLength, 20,
                        NULL);
    }
  }
  else
#endif
    for (i = 0; i < MAX_COOR; i ++)
    {
        labels[i] = XtVaCreateManagedWidget ("", xmLabelGadgetClass, rc ,
		      XmNcolumns, 2,
                      XmNmarginTop , 7,
                      XmNmarginBottom , 7,
                        NULL); 
        coors[i][0] = XtVaCreateManagedWidget ("", xmTextFieldWidgetClass, rc2, 
		      XmNmarginLeft , 5,
		      XmNmaxLength, 20,
                        NULL);
        coors[i][1] = XtVaCreateManagedWidget ("", xmTextFieldWidgetClass, rc2, 
		      XmNmarginLeft , 5,
		      XmNmaxLength, 20,
                        NULL);
    }
		      /*XmNancestorSensitive, True,*/
    XtAddCallback(coorpop, XmNokCallback, downcb, coorpop);
    XtAddCallback(coorpop, XmNokCallback, action_taken, 1);
    XtAddCallback(coorpop, XmNcancelCallback, action_taken, 2);
    XtAddCallback(coorpop, XmNcancelCallback, downcb, coorpop);
    XtAddCallback(coorpop, XmNcancelCallback, make_monolog,
	       "Registration cancelled. No new points are registered!");

    XtUnmanageChild (XmSelectionBoxGetChild(coorpop, XmDIALOG_HELP_BUTTON));
    XtUnmanageChild (XmSelectionBoxGetChild(coorpop, XmDIALOG_COMMAND_TEXT));

    set_coors();

}
   


set_coors()
{
    int i;
    char buf[100], s[10];
    XmString t = XmStringCreateSimple("*"), num;

#ifdef LATLON
  if (ll_flag) {
    for (i = 0; i < MAX_COOR; i++)
    {
	if (use[i])
	    XtVaSetValues (labels[i], XmNlabelString ,t, NULL);
        else
	{
	    sprintf (s, "%d", i+1);
	    num = XmStringCreateSimple(s);
	    XtVaSetValues (labels[i], XmNlabelString ,num, NULL);
	    XtFree (num);
        }
	if (bcx[i][0]!='\0')
	{
	    XmTextSetString (coors[i][0], bcx[i]);
	    XtVaSetValues (coors[i][0],  XmNcursorPosition, 0, NULL);
	}
        else {
	    XmTextSetString (coors[i][0], "");
	    XtVaSetValues (coors[i][0],  XmNcursorPosition, 0, NULL);
        }

	if (bcy[i][0]!='\0')
	{
	    XmTextSetString (coors[i][1], bcy[i]);
	    XtVaSetValues (coors[i][1],  XmNcursorPosition, 0, NULL);
	}
        else {
	    XmTextSetString (coors[i][1], "");
	    XtVaSetValues (coors[i][1],  XmNcursorPosition, 0, NULL);
        }
     }
  }
  else
#endif

    for (i = 0; i < MAX_COOR; i++)
    {
	if (use[i])
	    XtVaSetValues (labels[i], XmNlabelString ,t, NULL);
        else
	{
	    sprintf (s, "%d", i+1);
	    num = XmStringCreateSimple(s);
	    XtVaSetValues (labels[i], XmNlabelString ,num, NULL);
	    XtFree (num);
	}
	if (bx[i])
	{
	    G_format_easting (bx[i], buf, -1); /* G_projection()); */
	    XmTextSetString (coors[i][0], buf);
	    XtVaSetValues (coors[i][0],  XmNcursorPosition, 0, NULL);
	}

	if (by[i])
	{
	    G_format_northing (by[i], buf, -1); /* G_projection()); */
	    XmTextSetString (coors[i][1], buf);
	    XtVaSetValues (coors[i][1],  XmNcursorPosition, 0, NULL);
	}
    }
}


get_coors()
{
    int i;
    char *buf;

    for (i = 0; i < MAX_COOR; i++)
    {
        buf = XmTextGetString (coors[i][0]);
        sscanf (buf, "%lf", &bx[i]);
	XtFree (buf);
        buf = XmTextGetString (coors[i][1]);
        sscanf (buf, "%lf", &by[i]);
	XtFree (buf);
    }
} 

int get_coors_ll()
{
    int i;
    char *buf;
    int ask_again = 0;
    double junky[MAX_COOR],junkx[MAX_COOR];

    for (i = 0; i < MAX_COOR; i++)
    {
        buf = XmTextGetString (coors[i][0]);
        G_strip(buf);
        sscanf (buf, "%s", bcx[i]);
	XtFree (buf);
        buf = XmTextGetString (coors[i][1]);
        G_strip(buf);
        sscanf (buf, "%s", bcy[i]);
	XtFree (buf);

        if ((bcx[i][0]!='\0') || (bcy[i][0]!='\0')) {
          if (G_scan_northing (bcx[i],junky+i,PROJECTION_LL) == 0)
          {
            fprintf(stderr,"Invalid latitude: %s\n",bcx[i]);
            ask_again = 1;
            bcx[i][0] ='\0'; /* wipe out incorrect entry */
          }          
          if (G_scan_easting (bcy[i],junkx+i,PROJECTION_LL) == 0)
          {
            fprintf(stderr,"Invalid longtitude: %s\n",bcy[i]);
            ask_again = 1;
            bcy[i][0]='\0'; /* wipe out incorrect entry */
          }
        } 
    }
    if (ask_again)  {
/*
      make_monolog(1, "TYPO -- please reenter coordinate");
*/
      return -1;
    }
    else  {
/*
      for (i = 0; i < MAX_COOR; i++)
      {
        do_conversion(junkx+i,junky+i,1);
        bx[i] = junkx[i];
        by[i] = junky[i];
      }
*/
      return 1;
    }
}
   
   
show_coor_ask(str)
    char *str;
{
    extern Widget toplevel;
    XmString t = XmStringCreateSimple(str);
    XmString t1;
    XEvent event;
    Window win1, win2;
#ifdef LATLON
    int repeat;
#endif
    int i, found;
    double x, y;
	   
    set_coors();
#ifdef LATLON 
 if (ll_flag) {
    t1 = XmStringCreateSimple("Latitude");
    XtVaSetValues (xlabel,  
	           XmNlabelString ,t1,
                        NULL);
    XtFree (t1);
    t1 = XmStringCreateSimple("Longtitude");
    XtVaSetValues (ylabel,  
	           XmNlabelString ,t1,
                        NULL);
    XtFree (t1);
 }
#endif


    XtVaSetValues (message,  XmNlabelString, t, NULL);
    XtManageChild (coorpop);
    win1 = XtWindow (coorpop);
    XmUpdateDisplay (toplevel);
    coor_action = found = 0;
    while (!coor_action)
    {
	/*
	coor_action = ask_all (&x, &y);
	if (coor_action > 2)
	    coor_action = 0;
	*/

        XmUpdateDisplay (toplevel);
        while (XCheckMaskEvent (dpy,
            ButtonPressMask | ButtonReleaseMask | ButtonMotionMask |
            KeyPressMask | KeyReleaseMask, &event))
        {
	for (i = 0; i < MAX_COOR; i++)
	{      
	    if ((event.xany.window == XtWindow (coors[i][0])) ||
		  (event.xany.window == XtWindow (coors[i][1])) ||
		  (event.xany.type == MotionNotify)) 
	    {
                found = 1;
		XtDispatchEvent (&event);
		break;
	    }
	}
        if (event.xany.window == win1) 
	{
	    found = 1;
            XtDispatchEvent (&event);
	}
	if (!found)
            XBell (dpy,50);
        }
    }
    XmStringFree (t);
#ifdef LATLON
  if (ll_flag)
    if (get_coors_ll() < 0) repeat = 1;
    else repeat = 0;
  else
#endif
    get_coors();
    XtUnmanageChild (coorpop);
    if (coor_action == 2)
        coor_action =  -1; 
#ifdef LATLON
    if (coor_action!= -1 && repeat == 1) return 2;
#else
    if (coor_action!= -1) return 2;
#endif

    return (coor_action);
}
