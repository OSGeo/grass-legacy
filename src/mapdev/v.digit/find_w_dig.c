/*  @(#)find_w_dig.c    1.0  03/18/91  */
/*
**--> created from find_w_mouse.c by RL Glenn  1/1990
*/

#define  FIND_WIDTH 2

#include <math.h>
#include "digit.h"
#include "raster.h"
#include "display_line.h"
#include "dig_curses.h"
#include "local_proto.h"

int 
find_node_with_dig (double *x, double *y, double thresh, char *header)
{
    int button;
    int node_num;
    double    ux1, uy1;
    char    buffer[64];

    _Clear_base ();
    _Write_base(10, header);
    _Write_base(12, "    Select:");
    _Write_base(13, "   button 1:   Choose node");
    _Write_base(14, "   button 2:   Abort/Quit");
     Write_base(15, "   button 3:   Accept chosen node");

    node_num = 0;
    while (1)
    {
	button = get_digitizer_button_xy (&ux1, &uy1);

	switch (button - 1)
	 {
	    case 1:			/*  choose this  */
		if (node_num)
		{
		    R_standard_color (dcolors[dig_node_color (CMap->Node[node_num].n_lines)]);
		    Blot (&(CMap->Node[node_num].x), &(CMap->Node[node_num].y));
		}
		*x = ux1;
		*y = uy1;

		if ( (node_num = dig_which_node (CMap, &ux1, &uy1, thresh))  < 0)
		    sprintf (buffer, " no node found ");
		else
		{
		    sprintf (buffer, " node#: %d", node_num);
		    R_standard_color (dcolors[CLR_HIGHLIGHT]);
		    Blot (&(CMap->Node[node_num].x), &(CMap->Node[node_num].y));
		}
		Write_info(1, buffer);
		break;

	    case 2:			/*  coordinates  */
		Clear_info ();
		_Clear_base ();
		return (-1);
		break;

	    case 3:			/*  quit this  */
		Clear_info ();
		_Clear_base ();
		return (node_num);
		break;
	 }

	sprintf(buffer," EAST:  %10.2f", ux1);
	    Write_info(3, buffer);
	sprintf(buffer," NORTH: %10.2f", uy1);
	    Write_info(4, buffer);
    }
}


int find_point_with_dig (double *x, double *y, int n_points, double thresh)
{
    int  i;
    int  button;
    int  point_num;
    double    ux1, uy1;
    char    buffer[64];


    _Clear_base ();
    sprintf(buffer, " Number of points on line: %d", n_points);
    _Write_base(10, buffer);
    _Write_base(12, "    Select:");
    _Write_base(13, "   button 1:   Choose point on line");
    _Write_base(14, "   button 2:   Abort/Quit");
     Write_base(15, "   button 3:   Accept chosen point");

    point_num = -1;

    while (1)
    {
	button = get_digitizer_button_xy (&ux1, &uy1);

	switch (button - 1)
	 {
	    case 1:			/*  choose this  */
		if (point_num > 0)
		{
		    R_standard_color (dcolors[CLR_ERASE]);
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

		Write_info(1, buffer);
		break;

	    case 2:			/*  coordinates  */
		Clear_info ();
		_Clear_base ();
		return (-1);
		break;

	    case 3:			/*  quit this  */
		Clear_info ();
		_Clear_base ();
		return (point_num);
		break;

	 }

	sprintf(buffer," EAST:  %10.2f", ux1);
	    Write_info(3, buffer);
	sprintf(buffer," NORTH: %10.2f", uy1);
	    Write_info(4, buffer);

    }
}


/* x = y = 0.0  on abort.  */
int new_point_with_dig (double *x, double *y, char *header)
{
    int  button;
    double    ux1, uy1;
    char    buffer[64];
    int prev;

    *x = 0.0 ;  *y = 0.0;

    _Clear_base ();
    _Write_base(10, header);
    _Write_base(12, "    Select:");
    _Write_base(13, "   button 1:   Choose this position");
    _Write_base(14, "   button 2:   Abort/Quit");
    Write_base (15, "   button 3:   Accept chosen point position");


    prev = 0;
    while (1)
    {
	button = get_digitizer_button_xy (&ux1, &uy1);

	switch (button - 1)
	 {
	    case 1:			/*  choose this  */
		if (prev)
		{
		    R_standard_color (dcolors[CLR_ERASE]);
		    _Blot (x, y);
		}
		prev = 1;
		*x = ux1;
		*y = uy1;
		R_standard_color (dcolors[CLR_HIGHLIGHT]);
		Blot (x, y);
		Write_info (1, " Point location:");
		break;

	    case 2:			/*  abort  */
		if (prev)
		{
		    R_standard_color (dcolors[CLR_ERASE]);
		    Blot (x, y);
		}
		*x = 0.0;
		*y = 0.0;
		Clear_info ();
		_Clear_base ();
		return (0);
		break;

	    case 3:			/*  quit this  */
		Clear_info ();
		_Clear_base ();
		return (0);
		break;

	 }

	sprintf(buffer," EAST:  %10.2f", ux1);
	    Write_info(3, buffer);
	sprintf(buffer," NORTH: %10.2f", uy1);
	    Write_info(4, buffer);
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
int 
find_line_with_dig (int type_mask, char *header, int (*call)())
{
    int button;
    int line;
    int ret;
    double    ux1, uy1;
    double    ux2, uy2;
    char    buffer[64];
    char type;

    _Clear_base ();
    _Write_base(10, header);
    if (type_mask == DOT)
    {
	_Write_base(12, "    Select:");
	_Write_base(13, "   button 1:   Choose site");
	_Write_base(14, "   button 2:   Abort/Quit");
	 Write_base(15, "   button 3:   Accept chosen site");
    }
    else
    {
	_Write_base(12, "    Select:");
	_Write_base(13, "   button 1:   Choose line");
	_Write_base(14, "   button 2:   Abort/Quit");
	 Write_base(15, "   button 3:   Accept chosen line");
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
		    display_line (type, &Gpoints, line, CMap);
		}

		/*  changed 6/21/89 dpg
		line = dig_point_to_line (CMap, ux1, uy1, type_mask);
		*/
		line = dig_point_by_line (CMap, ux1, uy1, ux2, uy2, type_mask);
		if (! line)
		{
		    sprintf (buffer, " no line found ");
		    Write_info(1, buffer);
		}
		else
		{
		    type = CMap->Line[line].type;
		    if(0 > V2_read_line(CMap, &Gpoints, line))
			return (0);
		    highlight_line (type, &Gpoints, line, CMap);
		    sprintf (buffer, " line#: %d,  category#: %d", 
				   line, CMap->Att[CMap->Line[line].att].cat);
		    Write_info(1, buffer);
		    ret = line;
		    if (call != NULL)
			if (0 > ((*call) (CMap, line)))
			    ret = 0;
		}
		break;

	    case 2:				/*  Abort this  */
		if (line)	/* replace last chosen line */
		{
		    display_line (type, &Gpoints, line, CMap);
		}
		return (0);
		break;

	    case 3:				/* return line */
		/*
		display_line (type, &Gpoints, line, CMap);
		*/
		Clear_info ();
		_Clear_base ();
		return (ret);
		break;
	 }

    }
}
