/*
**  Last modified by Dave Gerdes  5/1988
**	added find_line, other changes for 3.0
**  US Army Construction Engineering Research Lab
** Last, Last modified by Terry Baker Spring 1993
** for compatibility w/ X
*/

#define  FIND_WIDTH 2
#include "digit.h"


static int screen_x = 1, screen_y = 1;
static int accpt=1;

set_accept(on)
    int on;
{
    accpt = on;
}
find_node_with_mouse ( x, y, thresh, header)
    double    *x, *y;
    double    thresh;
    char    *header;
{
    int node_num;
    double    ux1, uy1;
    double    ux2, uy2;
    char    buffer[64];

   
    debugf ("find_node_w_mouse \n");
    if (accpt)
        show_select_dialog("accept","abort", header, 1);
    else
        show_select_dialog(NULL, "abort", header, 0);
    node_num = 0;
    while (1)
    {
	 switch (Check_for_action(&screen_x, &screen_y)) {
	     case FIND:
		if (node_num > 0)
		{
		    standard_color (dcolors[dig_node_color (CM->Node[node_num].n_lines)]);
		    Blot (&(CM->Node[node_num].x), &(CM->Node[node_num].y));
		}
		screen_to_utm(screen_x, screen_y, &ux1, &uy1);
		*x = ux1;
		*y = uy1;

		if ( (node_num = dig_which_node (CM, &ux1, &uy1, thresh))  < 0)
		    write_info (1, " no node found ");
		else
		{
		    sprintf (buffer, " node#: %d", node_num);
		    standard_color (dcolors[CLR_HIGHLIGHT]);
		    Blot (&(CM->Node[node_num].x), &(CM->Node[node_num].y));
		    write_info(1, buffer);
		}
	screen_to_utm(screen_x, screen_y, &ux2, &uy2);

	sprintf(buffer," EAST:  %10.2f   NORTH: %10.2f", 
		   ux1>ux2?ux1:ux2,  uy1>uy2?uy1:uy2);
	add_info(1, buffer);
	        break;

	     case ACCEPT:  /*  coordinates  */
		return (node_num);
	         break;

	     case DONE:  /*  coordinates  */
		/* quit this  */
		return (-1);
	         break;
	     
	     default:
	         break;
	}
    }
}


find_point_with_mouse ( x, y, n_points, thresh)
    double    *x, *y;
    int    n_points;
    double    thresh;
{
    int  i = 0;	/* assignment only to shut up saber */
    int  point_num;
    double    ux1, uy1;
    double    ux2, uy2;
    double    fabs();
    char    buffer[64];


    sprintf(buffer, " Number of points on line: %d  Choose a point.", n_points);
    if (accpt)
        show_select_dialog("accept","abort", buffer, 1);
    else
        show_select_dialog(NULL, "abort", buffer, 0);

    point_num = -1;

    while (1)
    {
	 switch (Check_for_action(&screen_x, &screen_y)) {
	     case FIND:

		if (point_num > 0)
		{
		    standard_color (dcolors[CLR_ERASE]);
		    Blot (x+i, y+i);
		}
		screen_to_utm(screen_x, screen_y, &ux1, &uy1);

	    /*  find the point in the line  */
		for ( i=0; i < n_points; i++ )
		    if (  (fabs( *(x+i) - ux1)  <= thresh)  &&
			  (fabs( *(y+i) - uy1)  <= thresh) ) 
			    break;


		if ( i < n_points)
		{
		    /*
		    ** assume a chosen line is already highlit
		    **  if Disp_lines, then make it contrast 
		    */
		    if (Disp_lines)
			standard_color (dcolors[CLR_1_NODE]);
		    else
			standard_color (dcolors[CLR_HIGHLIGHT]);
		    Blot (x+i, y+i);
		    point_num = i;
		    screen_to_utm(screen_x, screen_y, &ux2, &uy2);

		    sprintf(buffer,"Point #: %d  East:  %10.2f   North: %10.2f",
			   i+1, ux1>ux2?ux1:ux2, uy1>uy2?uy1:uy2);
		}
		else
		{
		    sprintf (buffer, " no point found ");
		    point_num = -1;
		}

		write_info(1, buffer);
	        break;

	    case ACCEPT:
		return (point_num);
		break;

	    case DONE:
		if (point_num > 0)
		{
		    standard_color (dcolors[CLR_ERASE]);
		    Blot (x+i, y+i);
		}
		return (-1);
		break;
	    
	    default:
		break;

	 }

    }
}


/* x = y = 0.0  on abort.  */
new_point_with_mouse (x, y, header)
    double    *x, *y;
    char *header;
{
    int  button;
    double    ux1, uy1;
    double    ux2, uy2;
    char    buffer[64];
    int prev;
    *x = 0.0 ;  *y = 0.0;
    if (accpt)
        show_select_dialog("accept","abort", header, 1);
    else
        show_select_dialog(NULL, "abort", header, 0);

    prev = 0;
    while (1)
    {
	    switch (Check_for_action(&screen_x, &screen_y)) {
	    case FIND:

		if (prev)
		{
		    standard_color (dcolors[CLR_ERASE]);
		    _Blot (x, y);
		}
		prev = 1;
		screen_to_utm(screen_x, screen_y, &ux1, &uy1);
		*x = ux1;
		*y = uy1;
		standard_color (dcolors[CLR_HIGHLIGHT]);
		Blot (x, y);
		write_info (1, " Point location:");

		screen_to_utm(screen_x, screen_y, &ux2, &uy2);

		sprintf(buffer," EAST:  %10.2f   NORTH: %10.2f", 
                      ux1>ux2?ux1:ux2, uy1>uy2?uy1:uy2);
		add_info(1, buffer);
	        break;

	    case ACCEPT:
                prev = 0;
		return (ACCEPT);
		break;

	    case DONE:
		if (prev)
		{
		    standard_color (dcolors[CLR_ERASE]);
		    Blot (x, y);
		}
		*x = 0.0;
		*y = 0.0;
		return (DONE);
		break;
	    
	    default:
		break;

	 }
    }
}

/* ask user to choose a line in the window with the mouse
**   if click finds a line, highlight it.
**   allow option to abort or select that line
**   on abort line is redisplayed with normal line drawing colors
**   on select, line number is returned and line is left highlighted
**
**   type_mask is line type mask to limit searches to   or -1 if all lines 
**      are to be searched
**   header  is title to give options menu
**
**   calls   Read_line ()
**     fills   extern   Gpoints  with  Xarray Yarray info.  if successful
**
**   call is a pointer to subroutine that gets called on each successful
**	choice of a line.   If call == NULL  no special action is taken
**	if call() returns (< 0)  -> find_line_ returns 0
**
**   always returns a positive line  or 0 on not found or aborted
**
*/
find_line_with_mouse (type_mask, header, call)
    char    *header;
    int type_mask;
    int (*call)();
{
    return (_find_line_with_mouse (type_mask, header, call, USE_PTR, 0., 0.));
}

/*
**  if type_ptr != USE_PTR, then must have x, and y values
**   otherwise, they are not necessary
*/
_find_line_with_mouse (type_mask, header, call, type_ptr, x, y)
    int type_mask;
    char    *header;
    int (*call)();
    int type_ptr;
    double x, y;
{
    int button;
    static int line;
    int ret;
    double    ux1, uy1;
    double    ux2, uy2;
    char    buffer[64];
    static int type;
    int screen_x1, screen_y1;
    char msg[50];
    int tmp;

    if (accpt)
        show_select_dialog("accept","abort", header, 1);
    else
        show_select_dialog(NULL, "abort", header, 0);
    
    if (type_ptr != USE_PTR)
	utm_to_screen (x, y, &screen_x1, &screen_y1);
   switch (type_mask) {
      case AREA:
          strcpy (msg, "area"); 
	  break;
      case DOT:
          strcpy (msg, "site"); 
	  break;
      default:
          strcpy (msg, "line"); 
	  break;
    }

    button = 0;
    ret = line = 0;
    while (1)
    {
	switch (type_ptr) {
	    case USE_PTR:
		get_location_with_pointer (&screen_x, &screen_y, &button);
		break;
	    case USE_LINE:
		get_location_with_line 
		    (screen_x1, screen_y1, &screen_x, &screen_y, &button);
		break;
	    case USE_BOX:
		get_location_with_box 
		     (&screen_x1, &screen_y1, &screen_x, &screen_y, &button);
		break;
	}
/* 
	** instead of just taking a point, lets take several pixels around 
	** the mouse also 
	**  determined by FIND_WIDTH.  if == 1, then use first square of
	**  pixels (3x3) around point.  if == 2, then uses a 5x5 pixel square
	**  to determine which lines to check.
	*/
	 switch (button) {
	     case FIND:
                
		/* Top left */
		screen_to_utm 
		    (screen_x-FIND_WIDTH, screen_y-FIND_WIDTH, &ux1, &uy1); 
		
		/* bottom right */
		screen_to_utm 
		    (screen_x+FIND_WIDTH, screen_y+FIND_WIDTH, &ux2, &uy2);


		/* Hack   dpg  12/6/89 */
		screen_to_utm (screen_x, screen_y, &Point_X, &Point_Y);

		if (line)	/* replace last chosen line */
		{
		    reset_lit_line();
		    display_line (type, &Gpoints, line, CM);
		}

		line = dig_point_by_line (CM, ux1, uy1, ux2, uy2, type_mask);
		if (! line)
		{
		    sprintf (buffer, "No %s found", msg);
		    write_info (1,buffer);
		    continue;
		}
		else
		{
		    type = CM->Line[line].type;
		    if(0 > V1_read_line(CM, &Gpoints, CM->Line[line].offset))
			return (0);
		    highlight_line (type, &Gpoints, line, CM);
		    set_lit_line (&Gpoints, type, line);

		    if (CM->Line[line].att)	/* for scs */
			sprintf (buffer, " Line#: %d  Category: %d", 
				 line, CM->Att[CM->Line[line].att].cat);
		    else
			sprintf (buffer, " Line#: %d  Category: (unlabeled)", 
					      line);
		    write_info(1, buffer);

		    ret = line;
		    if (call != NULL)
			if (0 > ((*call) (CM, line)))
			    ret = 0;
		}
		break;
	    case DONE:
		if (line)	/* replace last chosen line */
		{
		    display_line (type, &Gpoints, line, CM);
		    line = 0;
		    reset_lit_line();
		}
		return (0);
		break;
	    case ACCEPT:
		if (line)
		{
		    reset_lit_line();
                    line = 0;
		    return (ret);
                }
		else
		{
		    sprintf (buffer, "No %s has been chosen", msg);
		    make_monolog (1, buffer);
                    return (0);
		}
		break;
	    default:
		break;
	 }

    }
}

   
show_select_dialog(ok, cancel, str, manage)
    char *ok, *cancel;
    char *str;
    int manage;
{
   extern Widget toplevel;

    XmString  message, accept, title, done;
    Widget w;
    extern Widget menubar;
    static int popxy[2];
    static int first = 1;

    if (first)
    {
        choose = XmCreatePromptDialog(canvas, "", NULL, 0);
	first = 0;
        XtManageChild (XmSelectionBoxGetChild(choose, XmDIALOG_APPLY_BUTTON));
        XtUnmanageChild (XmSelectionBoxGetChild(choose, XmDIALOG_HELP_BUTTON));
        XtUnmanageChild (XmSelectionBoxGetChild(choose, XmDIALOG_OK_BUTTON));
        XtUnmanageChild (XmSelectionBoxGetChild(choose, XmDIALOG_COMMAND_TEXT));
        XtAddCallback (choose, XmNcancelCallback, reset_lit_line, NULL);
        XtAddCallback (choose, XmNcancelCallback, actioncb, DONE);
        XtAddCallback (choose, XmNcancelCallback, downcb, choose);
    }
   

    
/* change labels */
    title = XmStringCreateSimple ("select");
    done = XmStringCreateSimple (cancel);
    message = XmStringCreateSimple (str);

    XtVaSetValues(choose, 
	    XmNcancelLabelString, done,
	    XmNselectionLabelString, message, 
	    XmNdialogTitle, title, 
	    NULL);
    

    XmStringFree (title);
    XmStringFree (message);
    XmStringFree (done);

    if(ok != NULL)
    {
        accept = XmStringCreateSimple (ok);
	XtVaSetValues (choose, XmNapplyLabelString, accept, NULL);
        XtManageChild (XmSelectionBoxGetChild(choose, XmDIALOG_APPLY_BUTTON));
	XtFree (accept);
    }
    else
    { 
       
        if(XtIsManaged (XmSelectionBoxGetChild(choose, XmDIALOG_APPLY_BUTTON)))
        XtUnmanageChild (XmSelectionBoxGetChild(choose, XmDIALOG_APPLY_BUTTON));
    }
    
    if (manage)
    {
        XtAddCallback (choose, XmNapplyCallback, downcb, choose);
    }
    else
    {
        XtRemoveAllCallbacks (choose, XmNapplyCallback);
    }
    XtAddCallback (choose, XmNapplyCallback, actioncb, ACCEPT);
/* manage dialog */
    XtManageChild (choose);

}

actioncb(w, i)
    Widget w;
    int i;
{
   action = i;
}



Check_for_action(x, y)
    int *x, *y;
{
    XEvent event;
    extern Widget toplevel, monolog;
    Window win1 = XtWindow (choose);
    Window win2 = XtWindow (canvas);
    Window win3 = XtWindow (monolog);

    XFlush (dpy);
    *x = 0;
    *y = 0;
    action = 0;
    XmUpdateDisplay (toplevel);
    while (XCheckMaskEvent (dpy,
	ButtonPressMask | ButtonReleaseMask | ButtonMotionMask
	| KeyPressMask | KeyReleaseMask, &event))
    {
	if ((event.xany.window == win1)|| (event.xany.window == win3))
	  XtDispatchEvent (&event);
	else if (event.xany.window == win2)
	{
	    *x = event.xbutton.x;
	    *y = event.xbutton.y;
	    if (event.xany.type == ButtonRelease)
	    {
		if ((*x >= 0) && (*y >= 0) 
		    && (*x <= screen_right) && (*y <= screen_bot))
	            action = FIND;
	    }
	    else if (event.xany.type == ButtonPress) 
	        action = DRAW;
	    else if (event.xany.type == MotionNotify)
	    {

	    	*x = event.xmotion.x;
	        *y = event.xmotion.y;
	        action = DRAW;
	    }
	}
	else 
	{
	    if (event.xany.type != MotionNotify)
	        XBell (dpy,50);
	}
    }
    return (action);
}
