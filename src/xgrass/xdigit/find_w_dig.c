/*  @(#)find_w_dig.c	5.1.1.3  6/16/93  */
/*  @(#)find_w_dig.c    1.0  03/18/91  */
/*
**--> created from find_w_mouse.c by RL Glenn  1/1990
*/

#define  FIND_WIDTH 2

#include "digit.h"
#include <math.h>

Widget digmenu;

find_node_with_dig ( x, y, thresh, header)
    double    *x, *y;
    double    thresh;
    char    *header;
{
    int button;
    int node_num;
    double    ux1, uy1;
    char    buffer[64];

    make_dig_menu(header,
		"   button 1:   Choose node",
                "   button 2:   Abort/Quit",
                "   button 3:   Accept chosen node");

    node_num = 0;
    while (1)
    {
	button = get_digitizer_button_xy (&ux1, &uy1);

	switch (button - 1)
	 {
	    case 1:			/*  choose this  */
		if (node_num)
		{
		    standard_color (dcolors[dig_node_color (CM->Node[node_num].n_lines)]);
		    Blot (&(CM->Node[node_num].x), &(CM->Node[node_num].y));
		}
		*x = ux1;
		*y = uy1;

		if ( (node_num = dig_which_node (CM, &ux1, &uy1, thresh))  < 0)
		    sprintf (buffer, " no node found ");
		else
		{
		    sprintf (buffer, " node#: %d", node_num);
		    standard_color (dcolors[CLR_HIGHLIGHT]);
		    Blot (&(CM->Node[node_num].x), &(CM->Node[node_num].y));
		}
		write_info(1, buffer);
		break;

	    case 2:			/*  coordinates  */
		end_menu();
		return (-1);
		break;

	    case 3:			/*  quit this  */
		end_menu();
		return (node_num);
		break;
	 }

	sprintf(buffer," EAST:  %10.2f", ux1);
	    write_info(1, buffer);
	sprintf(buffer," NORTH: %10.2f", uy1);
	    add_info(1, buffer);
    }
}


find_point_with_dig ( x, y, n_points, thresh)
    double    *x, *y;
    int    n_points;
    double    thresh;
{
    int  i;
    int  button;
    int  point_num;
    double    ux1, uy1;
    char    buffer[64];


    sprintf(buffer, " Number of points on line: %d", n_points);
    make_dig_menu(buffer,
    	    "   button 1:   Choose point on line",
    	    "   button 2:   Abort/Quit",
     	    "   button 3:   Accept chosen point");

    point_num = -1;

    while (1)
    {
	button = get_digitizer_button_xy (&ux1, &uy1);

	switch (button - 1)
	 {
	    case 1:			/*  choose this  */
		if (point_num > 0)
		{
		    standard_color (dcolors[CLR_ERASE]);
		    Blot (x+i, y+i);
		}

	    /*  find the point in the line  */
		for ( i=0; i < n_points; i++ )
		    if (  (fabs( *(x+i) - ux1)  <= thresh)  &&
			  (fabs( *(y+i) - uy1)  <= thresh) ) 
			    break;


		if ( i < n_points)
		{
		    sprintf (buffer, " point#: %d", i+1);
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
		}
		else
		{
		    sprintf (buffer, " no point found ");
		    point_num = -1;
		}

		write_info(1, buffer);
		break;

	    case 2:			/*  coordinates  */
		end_menu();
		return (-1);
		break;

	    case 3:			/*  quit this  */
		end_menu();
		return (point_num);
		break;

	 }

	sprintf(buffer," EAST:  %10.2f", ux1);
	    write_info(1, buffer);
	sprintf(buffer," NORTH: %10.2f", uy1);
	    add_info(1, buffer);

    }
}


/* x = y = 0.0  on abort.  */
new_point_with_dig (x, y, header)
    double    *x, *y;
    char *header;
{
    int  button;
    double    ux1, uy1;
    double    ux2, uy2;
    char    buffer[64];
    int prev;

    *x = 0.0 ;  *y = 0.0;

    make_dig_menu (header,
    	    "   button 1:   Choose this position",
    	    "   button 2:   Abort/Quit",
    	    "   button 3:   Accept chosen point position");


    prev = 0;
    while (1)
    {
	button = get_digitizer_button_xy (&ux1, &uy1);

	switch (button - 1)
	 {
	    case 1:			/*  choose this  */
		if (prev)
		{
		    standard_color (dcolors[CLR_ERASE]);
		    _Blot (x, y);
		}
		prev = 1;
		*x = ux1;
		*y = uy1;
		standard_color (dcolors[CLR_HIGHLIGHT]);
		Blot (x, y);
		write_info (1, " Point location:");
		break;

	    case 2:			/*  abort  */
		if (prev)
		{
		    standard_color (dcolors[CLR_ERASE]);
		    Blot (x, y);
		}
		*x = 0.0;
		*y = 0.0;
		end_menu();
		return (0);
		break;

	    case 3:			/*  quit this  */
		end_menu();
		return (0);
		break;

	 }

	sprintf(buffer," EAST:  %10.2f", ux1);
	    add_info(1, buffer);
	sprintf(buffer," NORTH: %10.2f", uy1);
	    add_info(1, buffer);
    }
}

/* ask user to choose a line in the window with the dig
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
find_line_with_dig (type_mask, header, call)
    char    *header;
    char type_mask;
    int (*call)();
{
    int button;
    int line;
    int ret;
    double    ux1, uy1;
    double    ux2, uy2;
    char    buffer[64];
    char type;

    if (type_mask == DOT)
    {
        make_dig_menu(header,
	"   button 1:   Choose site",
	"   button 2:   Abort/Quit",
	"   button 3:   Accept chosen site");
    }
    else
    {
    make_dig_menu(header,
	"   button 1:   Choose line",
	"   button 2:   Abort/Quit",
	"   button 3:   Accept chosen line");
    }

    ret = line = 0;
    while (1)
    {
	button = get_digitizer_button_xy (&ux1, &uy1);
	/* 
	** instead of just taking a point, lets define a window around
	** what the user is pointing to
	**  determined by FIND_WIDTH.  if == 1, then use 50 meter window
	**  around point.  if == 2, then uses a 100 meter window
	**  to determine which lines to check.
	*/
        ux2 = ux1 + FIND_WIDTH * 50;
        uy2 = uy1 + FIND_WIDTH * 50;
        ux1 = ux1 - FIND_WIDTH * 50;
	uy1 = uy1 - FIND_WIDTH * 50;

	switch (button - 1)
	 {
	    case 1:			/*  choose this  */
		if (line)	/* replace last chosen line */
		{
		    display_line (type, &Gpoints, line, CM);
		}

		/*  changed 6/21/89 dpg
		line = dig_point_to_line (CM, ux1, uy1, type_mask);
		*/
		line = dig_point_by_line (CM, ux1, uy1, ux2, uy2, type_mask);
		if (! line)
		{
		    sprintf (buffer, " no line found ");
		    write_info(1, buffer);
		}
		else
		{
		    type = CM->Line[line].type;
		    if(0 > V2_read_line(CM, &Gpoints, line))
		    {
    			end_menu();
			return (0);
		    }
		    highlight_line (type, &Gpoints, line, CM);
		    sprintf (buffer, " line#: %d,  category#: %d", 
				   line, CM->Att[CM->Line[line].att].cat);
		    write_info(1, buffer);
		    ret = line;
		    if (call != NULL)
			if (0 > ((*call) (CM, line)))
			    ret = 0;
		}
		break;

	    case 2:				/*  Abort this  */
		if (line)	/* replace last chosen line */
		{
		    display_line (type, &Gpoints, line, CM);
		}
    		end_menu();
		return (0);
		break;

	    case 3:				/* return line */
    		end_menu();
		return (ret);
		break;
	 }

    }
}

make_dig_menu(header, line1, line2, line3)
    char *header;
    char *line1;
    char *line2;
    char *line3;
{
    int n, i; 
    Arg wargs[10];
    XmString  message, title, tmp[3];
    extern Widget toplevel;
    Widget rc, labels[3];
    
    message = XmStringCreateSimple (header);
    title = XmStringCreateSimple ("");
    tmp[0] = XmStringCreateSimple (line1);
    tmp[1] = XmStringCreateSimple (line2);
    tmp[2] = XmStringCreateSimple (line3);

    n = 0;
    XtSetArg (wargs[n], XmNmessageString, message); n++;
    XtSetArg (wargs[n], XmNdialogTitle, title); n++;
    digmenu = XmCreateMessageDialog (toplevel, "", wargs, n);
    rc = XtVaCreateManagedWidget ("rc", xmRowColumnWidgetClass, digmenu,
       NULL);
    for (i = 0; i < 3; i ++)
    {
        labels[i] = XtVaCreateManagedWidget ("", xmLabelWidgetClass, rc,
		  XmNlabelString , tmp[i], NULL);
	XtFree (tmp[i]);
    }

    XtVaSetValues (XtParent(digmenu), 
		XmNsaveUnder, True,
		NULL);

    XmStringFree (message);
    XmStringFree (title);

    XtUnmanageChild (XmMessageBoxGetChild(digmenu, XmDIALOG_HELP_BUTTON));
    XtUnmanageChild (XmMessageBoxGetChild(digmenu, XmDIALOG_OK_BUTTON));
    XtUnmanageChild (XmMessageBoxGetChild(digmenu, XmDIALOG_CANCEL_BUTTON));

    XtManageChild (digmenu);
    XFlush (dpy);
    XmUpdateDisplay (toplevel);
}
end_menu()
{
    
    XtUnmanageChild (digmenu);
    XtDestroyWidget(digmenu);
}

