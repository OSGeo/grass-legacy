/*
**  Last modified by Dave Gerdes  5/1988
**	added find_line, other changes for 3.0
**  US Army Construction Engineering Research Lab
*/
/* modified by RL Glenn  12/1991
** USDA, SCS, Tech. Infor. Sys. Division
*/


#define  FIND_WIDTH 2

#include "digit.h"
#include "popup.h"

static int screen_x = 1, screen_y = 1;

find_node_with_mouse ( x, y, thresh, header)
    double    *x, *y;
    double    thresh;
    char    *header;
{
    int button;
    int node_num;
    double    ux1, uy1;
    double    ux2, uy2;
    char    buffer[64];
    int background_color, text_color, menu_left, menu_top;
    int div_color, ret, chr, first=1;

    menu_left = Next_l + 1;
    menu_top = Next_t + 9;

    buttons[0] = (char *) malloc (strlen (header) + 1);
    sprintf(buttons[0],"%s", header);
    buttons[1] = "Buttons:";
    buttons[2] = "Left:   Choose node";
    buttons[3] = "Middle: Abort/Quit";
    buttons[4] = "Right:  Accept chosen node";
    buttons[5] = "  ";
    buttons[6] = '\0';

    Dchoose(MEN.name) ;
    popup_butns( menu_top, menu_left, buttons, "find_nod", 1) ;
    Dchoose(DIG.name) ;

    node_num = 0;
    while (1)
    {
	R_get_location_with_pointer ( &screen_x, &screen_y, &button);
	flush_keyboard ();

	switch (button)
	 {
	    case 1:			/*  choose this  */
		if (node_num)
		{
		    R_standard_color (dcolors[dig_node_color (CM->Node[node_num].n_lines)]);
		    Blot (&(CM->Node[node_num].x), &(CM->Node[node_num].y));
		}
		screen_to_utm(screen_x, screen_y, &ux1, &uy1);
		*x = ux1;
		*y = uy1;

		if ( (node_num = dig_which_node (CM, &ux1, &uy1, thresh))  < 0)
		    sprintf (buffer, " no node found ");
		else
		{
		    sprintf (buffer, " node#: %d", node_num);
		    R_standard_color (dcolors[CLR_HIGHLIGHT]);
		    Blot (&(CM->Node[node_num].x), &(CM->Node[node_num].y));
		}
                message[0] = (char *) malloc (strlen (buffer) + 1);
                sprintf(message[0],"%s", buffer);
		break;

	    case 2:			/*  coordinates  */
                erase_popup("info");
                erase_popup("find_nod");
		return (-1);
		break;

	    case 3:			/*  quit this  */
                erase_popup("info");
                erase_popup("find_nod");
		return (node_num);
		break;
	 }

	screen_to_utm(screen_x, screen_y, &ux2, &uy2);

        sprintf(buffer,"EAST:%12.2lf  NORTH:%13.2lf .",
	      ux1>ux2?ux1:ux2,  uy1>uy2?uy1:uy2);
        if (first)
	    message[1] = (char *) malloc (strlen (buffer) + 1);

        sprintf(message[1],"%s", buffer);
        message[2] = " ";
        message[3] = '\0';

        Dchoose(MEN.name) ;
        popup_messg( "info", first) ;
	first = 0;
    }
}


find_point_with_mouse ( x, y, n_points, thresh)
    double    *x, *y;
    int    n_points;
    double    thresh;
{
    int  i;
    int  button;
    int  point_num;
    double    ux1, uy1;
    double    ux2, uy2;
    double    fabs();
    char    buffer[64];
    int menu_left, menu_top;
    int ret, chr, first=1;

    menu_left = Next_l + 1;
    menu_top = Next_t;

    sprintf(buffer, "# of points on line: %d", n_points);
    buttons[0] = (char *) malloc (strlen (buffer) + 1);
    sprintf(buttons[0],"%s", buffer);
    buttons[1] = "Buttons:";
    buttons[2] = "Left:   Choose point on line";
    buttons[4] = "Middle:  Abort/Quit";
    buttons[3] = "Right: Accept chosen point";
    buttons[5] = "  ";
    buttons[6] = '\0';

    G_clear_screen ();
    Dchoose(MEN.name) ;
    popup_butns(
                menu_top,
                menu_left,
                buttons,
		"find_pt",
		1
                ) ;
    Dchoose(DIG.name) ;

    point_num = -1;

    while (1)
    {
	R_get_location_with_pointer ( &screen_x, &screen_y, &button);
	flush_keyboard ();

	switch (button)
	 {
	    case 1:			/*  choose this  */
		if (point_num > 0)
		{
		    R_standard_color (dcolors[CLR_ERASE]);
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
		    sprintf (buffer, " point#: %d", i+1);
		    /*
		    ** assume a chosen line is already highlit
		    **  if Disp_lines, then make it contrast 
		    */
		    if (Disp_lines)
			R_standard_color (dcolors[CLR_1_NODE]);
		    else
			R_standard_color (dcolors[CLR_HIGHLIGHT]);
		    Blot (x+i, y+i);
		    point_num = i;
		}
		else
		{
		    sprintf (buffer, " no point found ");
		    point_num = -1;
		}

                message[0] = (char *) malloc (strlen (buffer) + 1);
                sprintf(message[0],"%s", buffer);
		break;

	    case 2:			/*  coordinates  */
		G_clear_screen ();
                erase_popup("info");
                erase_popup("find_pt");
		return (-1);
		break;

	    case 3:			/*  quit this  */
		G_clear_screen ();
                erase_popup("info");
                erase_popup("find_pt");
		return (point_num);
		break;

	 }

	screen_to_utm(screen_x, screen_y, &ux2, &uy2);

        sprintf(buffer,"EAST:%12.2lf  NORTH:%13.2lf .",
	      ux1>ux2?ux1:ux2,  uy1>uy2?uy1:uy2);
        if (first)
	    message[1] = (char *) malloc (strlen (buffer) + 1);

        sprintf(message[1],"%s\0", buffer);
        message[2] = " ";
        message[3] = '\0';

        Dchoose(MEN.name) ;
        popup_messg( "info", first) ;
	first = 0;

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
    int menu_left, menu_top;
    int ret, chr, first=1;

    menu_left = Next_l + 1;
    menu_top = Next_t;

    *x = 0.0 ;  *y = 0.0;

    buttons[0] = (char *) malloc (strlen (header) + 1);
    sprintf(buttons[0],"%s", header);
    buttons[1] = "Buttons:";
    buttons[2] = "Left:   Choose this position";
    buttons[3] = "Middle:  Abort/Quit";
    buttons[4] = "Right: Accept chosen point position";
    buttons[5] = "  ";
    buttons[6] = '\0';

    Dchoose(MEN.name) ;
    popup_butns(
                menu_top,
                menu_left,
                buttons,
		"find_n_pt",
		1
                ) ;
    Dchoose(DIG.name) ;

    prev = 0;
    while (1)
    {
	R_get_location_with_pointer ( &screen_x, &screen_y, &button);
	flush_keyboard ();

	switch (button)
	 {
	    case 1:			/*  choose this  */
		if (prev)
		{
		    R_standard_color (dcolors[CLR_ERASE]);
		    _Blot (x, y);
		}
		prev = 1;
		screen_to_utm(screen_x, screen_y, &ux1, &uy1);
		*x = ux1;
		*y = uy1;
		R_standard_color (dcolors[CLR_HIGHLIGHT]);
		Blot (x, y);
		break;

	    case 2:			/*  abort  */
		if (prev)
		{
		    R_standard_color (dcolors[CLR_ERASE]);
		    Blot (x, y);
		}
		*x = 0.0;
		*y = 0.0;
                if (first == 0)erase_popup("info");
                erase_popup("find_n_pt");
		return (0);
		break;

	    case 3:			/*  quit this  */
                if (first == 0)erase_popup("info");
                erase_popup("find_n_pt");
		return (0);
		break;

	 }

	screen_to_utm(screen_x, screen_y, &ux2, &uy2);

        sprintf(buffer,"EAST:%12.2lf  NORTH:%13.2lf .",
	      ux1>ux2?ux1:ux2,  uy1>uy2?uy1:uy2);
        if (first)
	    message[0] = (char *) malloc (strlen (buffer) + 1);

        sprintf(message[0],"%s\0", buffer);
        message[1] = " ";
        message[2] = '\0';

        Dchoose(MEN.name) ;
        popup_messg( "info", first) ;
	first = 0;
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
    char    *header;
    int type_mask;
    int (*call)();
    int type_ptr;
    double x, y;
{
    int button;
    int line;
    double    ux1, uy1;
    double    ux2, uy2;
    char    buffer[64], name_type[10];
    int type;
    int screen_x1, screen_y1;
    int mouse=1, menu_left, menu_top;
    int ret, chr, first=1;

    menu_left = Next_l + 1;
    menu_top = Next_t;

    buttons[0] = (char *) malloc (strlen (header) + 1);
    sprintf(buttons[0],"%s", header);
    buttons[1] = "Buttons:";

    if (type_mask == DOT)
    {
        buttons[2] = "Left:   Choose site";
        buttons[3] = "Middle: Abort/Quit";
        buttons[4] = "Right:  Accept chosen site";
	sprintf(name_type,"site#");
    }
    else
    {
        buttons[2] = "Left:   Choose line";
        buttons[3] = "Middle: Abort/Quit";
        buttons[4] = "Right:  Accept chosen line";
	sprintf(name_type,"line#");
    }
    buttons[5] = "  ";
    buttons[6] = '\0';

    Dchoose(MEN.name) ;
    popup_butns(
                menu_top,
                menu_left,
                buttons,
		"find_lin",
		1
                ) ;
    Dchoose(DIG.name) ; 

    if (type_ptr != USE_PTR)
	utm_to_screen (x, y, &screen_x1, &screen_y1);

    ret = line = 0;
    while (1)
    {
	switch (type_ptr) {
	    case USE_PTR:
		R_get_location_with_pointer (&screen_x, &screen_y, &button);
		break;
	    case USE_LINE:
		R_get_location_with_line (screen_x1, screen_y1, &screen_x, &screen_y, &button);
		break;
	    case USE_BOX:
		R_get_location_with_box (screen_x1, screen_y1, &screen_x, &screen_y, &button);
		break;
	}
	flush_keyboard ();
	/* 
	** instead of just taking a point, lets take several pixels around 
	** the mouse also 
	**  determined by FIND_WIDTH.  if == 1, then use first square of
	**  pixels (3x3) around point.  if == 2, then uses a 5x5 pixel square
	**  to determine which lines to check.
	*/
	screen_to_utm (screen_x-FIND_WIDTH, screen_y-FIND_WIDTH, &ux1, &uy1); /* Top left */
	screen_to_utm (screen_x+FIND_WIDTH, screen_y+FIND_WIDTH, &ux2, &uy2); /* bottom right */

        if (!first)
	   { /* erase the popup_messg */
	   Dchoose(MEN.name) ;
           erase_popup("info");
	   Dchoose(DIG.name) ;
	   } 
	
	switch (button)
	 {
	    case 1:			/*  choose this  */

		/* Hack   dpg  12/6/89 */
		screen_to_utm (screen_x, screen_y, &Point_X, &Point_Y);

		if (line)	/* replace last chosen line */
		{
		    display_line (type, &Gpoints, line, CM);
		}

/*DEBUG*/  debugf ("Calling dig_point_by_line ()\n");
		line = dig_point_by_line (CM, ux1, uy1, ux2, uy2, type_mask);

		if (! line)
		{
                if (type_mask == DOT)
		    sprintf (buffer, " no site found ");
                else
		    sprintf (buffer, " no line found ");
		}
		else
		{
		    type = CM->Line[line].type;
/*DEBUG*/ debugf ("calling V1_read_line from find_line ()\n");
		    if(0 > V1_read_line(CM, &Gpoints, CM->Line[line].offset))
			return (0);
		    highlight_line (type, &Gpoints, line, CM);

		    if (CM->Line[line].att)	/* for scs */
			sprintf (buffer, " %s: %d  Category: %d .", name_type, line, CM->Att[CM->Line[line].att].cat);
		    else
			sprintf (buffer, " %s: %d  Category: (unlabeled.)", name_type, line);

		    ret = line;
		    if (call != NULL)
			if (0 > ((*call) (CM, line)))
			    ret = 0;
		}
		if (ret == 0)
		    {
                    message[0] = (char *) malloc (strlen (buffer) + 1);
                    sprintf(message[0],"%s", buffer);
                    message[1] = " ";
                    message[2] = '\0';

                    Dchoose(MEN.name) ;
                    popup_messg( "info", 1) ;
	            first = 0;
		    }

                Dchoose(DIG.name) ;
		break;

	    case 2:				/*  Abort this  */
		if (line)	/* replace last chosen line */
		{
		    display_line (type, &Gpoints, line, CM);
		}
                if (!first) erase_popup("info");
                erase_popup("find_lin");
		return (0);
		break;

	    case 3:				/* return line */
                if (!first) erase_popup("info");
                erase_popup("find_lin");
		return (ret);
		break;
	 }
    }
}
