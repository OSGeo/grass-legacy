/*
**  Last modified by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
**
**  Last modified by Dave Gerdes  1/90  for dig_head stuff
*/


#include <stdio.h>
#include "digit.h"
#include "dig_head.h"

static  double  scale ;
double dig_unit_conversion ();

/*  functions calculate mapscale, get map scale and check it with the user  */

check_scale(Map)
	struct Map_info *Map;
{

	int		cur_scale ;
	double	calc_scale ;
	char	buf[80] ;
	double	fabs() ;
	double	get_map_scale() ;
/*DEBUG*/ debugf ("check_scale\n");

	calc_scale = get_map_scale() ;  	 /*  our calculated scale  */
	cur_scale = Map->head.orig_scale ;


/*	while (1)*/
	 {
	/*  scales are within 10% of each other, accept it  */
		if ( fabs( (double)(cur_scale - calc_scale))  <  (cur_scale * .1) )
		{
			/*
			head.orig_scale = calc_scale ;
			*/
			Map->head.orig_scale = cur_scale ;
			Vect__write_head_binary (Map, &(Map->head)) ;
			return (1) ;
	
		}
	
		cur_scale = ask_scale(cur_scale, calc_scale);

		if (cur_scale < 0)			/*  just hit return  */
		{
		    Map->head.orig_scale = calc_scale ;
		}
		/* 3.1 -dpg */
		Vect__write_head_binary (Map, &(Map->head)) ;
		return (1) ;

	 }

}


calculate_map_scale ()
{
	int	i ;
	int	screen_no ;
	char	*ptr ;

	double	x1, y1 ;

	double	x2, y2 ;
	double	X, Y ;
	double	hypot_x,  hypot_y,  hypot_avg ;
	float	d_scale ;		/*  digitizer scale  */

	double	hypot() ;
	double	fabs() ;


	transform_a_into_b ( (double)0, (double)0, &X, &Y) ;
	transform_a_into_b ( (double)10, (double)0, &x1, &y1) ;
	transform_a_into_b ( (double)0, (double)10, &x2, &y2) ;

	hypot_x = hypot ( fabs(x1-X), fabs(y1-Y)) ;
	hypot_y = hypot ( fabs(x2-X), fabs(y2-Y)) ;

	hypot_avg = (hypot_x + hypot_y) / 2.0 ;

	/*  
	*	convert digitizer units (inches) to meters by multipling by .0254 and 
	*	multiply by 10 for the number of digiter units used to compute the
	*	hypot average.	0.0254 * 10 = 0.254
	*/

	D_get_scale (&d_scale) ;		/*  get digitizer scale  */

	scale = hypot_avg / ( d_scale * (10. * dig_unit_conversion ())) ;
	/*
	scale = hypot_avg / ( d_scale * .254) ;
	*/
		
}


double
get_map_scale()
{
	return (scale) ;
}





static int done;

scale_done (w, num)
    Widget w;
    int num;
{
    done = num;
}



int
ask_scale (curr, calc)
    int curr;
    double calc;
{
    char buf[100];
    Widget ask, scaleform, labels[3];
    XmString title = XmStringCreateSimple ("Scale");
    XmString message, value;
    Arg wargs[5];
    int n=0;
    XmString t, s[3];
    XEvent event;
    Window win1, win2;
    extern Widget toplevel;
    static char *answer;
    double ret;
    double x, y;

    done = 0;
    ret = 0;
   
    sprintf (buf, "%lf", calc);
    value = XmStringCreateSimple (buf);
    s[0] = XmStringCreateSimple ( " Map scales are not close to each other."); 
    sprintf (buf,"Current map scale: %d.   Calculated map scale: %12lf", 
						       curr, calc) ;
    s[1] = XmStringCreateSimple ( buf); 
    s[2] = XmStringCreateSimple ( "Enter new scale and click 'ok'."); 
    
    message = XmStringCreateSimple 
   ("Click cancel or any cursor <key> to use calculated scale.");
    
    ask = XmCreatePromptDialog (toplevel, "", NULL, 0);
    XtVaSetValues (ask, XmNdialogTitle, title,
		XmNselectionLabelString, message,
		XmNtextString, value,
		 NULL);

    scaleform = XtVaCreateManagedWidget ("", xmRowColumnWidgetClass, ask, 
		   XmNentryAlignment, XmALIGNMENT_CENTER, NULL) ;
    labels[0]= XtVaCreateManagedWidget ("", xmLabelWidgetClass, scaleform, 
			 XmNlabelString, s[0], NULL) ;
    labels[1]= XtVaCreateManagedWidget ("", xmLabelWidgetClass, scaleform, 
			 XmNlabelString, s[1], NULL) ;
    labels[2]= XtVaCreateManagedWidget ("", xmLabelWidgetClass, scaleform, 
			 XmNlabelString, s[2], NULL) ;
    
    XmStringFree (title);
    XmStringFree (message);
    XmStringFree (s[0]);
    XmStringFree (s[1]);
    XmStringFree (s[2]);
    XmStringFree (value);

    
    XtAddCallback (ask, XmNokCallback, scale_done, 1);
    XtAddCallback (ask, XmNcancelCallback, scale_done, 2);
    XtUnmanageChild(XmSelectionBoxGetChild(ask, XmDIALOG_HELP_BUTTON));


    XtManageChild (ask);
    XmUpdateDisplay (toplevel);
    XFlush (XtDisplay (toplevel));
    clear();

    while (!done) 
    {
        XmUpdateDisplay (toplevel);
        XFlush (XtDisplay (toplevel));
	if (done = _coll_a_pnt ( &x, &y))
	    break;

    	    win1 = XtWindow (ask);
            win2 = XtWindow (XmSelectionBoxGetChild(ask, XmDIALOG_COMMAND_TEXT));
        while (XCheckMaskEvent (dpy,
            ButtonPressMask | ButtonReleaseMask |
            KeyPressMask | KeyReleaseMask, &event))
        {
        if ((event.xany.window == win1) || (event.xany.window == win2))
          XtDispatchEvent (&event);
        else
            XBell (dpy,50);
        XFlush (XtDisplay (toplevel));
        }
    }
    if (done == 1)
    {
	answer = 
	   XmTextGetString (XmSelectionBoxGetChild(ask, XmDIALOG_COMMAND_TEXT));
        ret= atoi (answer);
	if (!ret)
	{
	    ret = -1;
	}

    }	
    else
	ret = -1;
    XtFree (answer);
    XtUnmanageChild (ask);
    XmUpdateDisplay (toplevel);

    return (ret);

}

