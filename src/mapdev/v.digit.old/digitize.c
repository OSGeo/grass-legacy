/*  @(#)digitize.c    2.1  6/26/87  */
/*
**  Last modified by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
*/

#include <stdio.h>
#include "digit.h"
#include "dig_head.h"

/* made these global so modes would remain set if they leave and return */
static char    type = AREA;
static int    mode = STREAM;
static int    hold = STREAM;
static int	auto_label = 0;

Digitize ()
{
    int command;
    int Pass;
    int ret;
    int chr;


    /* if mouse digitizing, STREAM mode is disabled */
    if (Digtiz_Device == MOUSE)
	mode = POINT;


    Pass = 0;
    Set_G_Mask (MG_DIGIT, OFF);

    while(1) 
    {
	_Clear_info ();
	update_global_menu ();
	_Write_dig_win();
	_Write_type_info();
	_show_mode(mode, type, auto_label);
	_Base_refresh ();

	if ((command = get_menu_command (&M_digit, &chr)) > 0)
	{
	    switch(command)
	    {
		case MDC_UNDO:
		    break;
		case MDC_REPLOT:
		    Replot_screen ();
		    _Write_type_info();
		    show_mode(mode, type, auto_label);
		    break;
		case MDC_QUIT:
		    goto DIGIT_END;
		    break;
		case MDC_MODE:
		    TOGGLE (mode);
		    show_mode(mode, type, auto_label);
		    break;
		case MDC_TYPE:
		    switch (type) {
			case LINE:
			    type = AREA;
			    break;
			case AREA:
			    type = DOT;
			    break;
			case DOT:
			    type = LINE;
			    break;
		    }
		    show_mode(type == DOT ? POINT : mode, type, auto_label);
		    break;
		case MDC_LABEL:
		    auto_label = ask_cat ();
		    show_mode(mode, type, auto_label);
		    break;
		case MDC_DIGIT:
		    do_digitize(CM, type == DOT ? POINT : mode, type, CM->prune_thresh);
		    _Write_type_info();
		    show_mode(mode, type, auto_label);
		    break;
		default:
		    break;
	    }
	}
	else
	{
	    if ((ret = global_menu (chr, &M_digit)) > 0)
	    {
		Pass = ret;
		break;  /* return and execute new command */
	    }
	    if (ret < 0)
		BEEP;
	}
    }
DIGIT_END:
    
    Set_G_Mask (MG_DIGIT, ON);
    return (Pass);
}


do_digitize(map, mode, type, sample_thresh)
    struct Map_info *map;
    int mode;
    char type;
    double sample_thresh;
{
    char buffer[64];
    int stream_mode;
    struct new_node node;
    int line;
    int area;
    int yes_no ;
    struct line_pnts Xpoints;



    Xpoints.alloc_points = 0;

    while(1)
    {
	Dig_menu_opts ();
	Clear_info();	 /*New*/

	/* if DOT, can use STOP digitizing to quit 
	*/
	if (type != DOT)
	{
	    if (Digtiz_Device == MOUSE)
	    {
		if (!mouse_yes_no ("Begin digitizing? "))
		    return (0);
	    }
	    else
	    if (D_cursor_buttons())
	    {
		if ( ! ask_driver_yes_no("Begin digitizing? ") )
			return(0) ;
	    }
	}
	else
        {
 	    _Clear_base () ;
	    Write_base(10, "Site digitizing") ;
        }

	if (Digtiz_Device == MOUSE)
	    stream_mode = mouse_collect_points(mode, type, &Xpoints);
	else
	    stream_mode = Collect_points(mode, type, &Xpoints);
	flush_keyboard ();

	/* 
	** requested to stop digitizing
	*/
	if (DOT && Xpoints.n_points == 0)
	    return (0);

    /*DEBUG*/ debugf ("Collected %d points \n", Xpoints.n_points);

	if (stream_mode)
	{
	    Xpoints.n_points = dig_prune(&Xpoints, sample_thresh);
    /*DEBUG*/ debugf ( "after prune:  %d points\n", Xpoints.n_points);
	}

	/*  toss out degenerate lines */
	if (type != DOT)
	{
	    if (Xpoints.n_points == 1 ||  /* are all points w/in snapping thresh? */
		 dig_is_line_degenerate (&Xpoints, map->head.map_thresh))
	    {
		Xpoints.n_points = 0;
		BEEP;
		Write_info(3, "Only 1 point digitized, Ignoring...");
		sleep (3);
		goto d_done;
	    }
	}
	else
	{
	    if (Xpoints.n_points == 1)
	    {
		dig_alloc_points (&Xpoints, 2);
		Xpoints.x[1] = Xpoints.x[0];
		Xpoints.y[1] = Xpoints.y[0];
		Xpoints.n_points = 2;
	    }
	}

	/* sites do not have nodes */
	/*
	if (type == DOT)
	{
	    node.N1 = 0;
	    node.N2 = 0;
	    node.cnt = 0;
	}
	else
	*/
	{
	    dig_check_nodes (map, &node, &Xpoints);
/*DEBUG*/ debugf ( "Check_nodes returns  N1 = %d  N2 = %d\n", node.N1, node.N2);
	}
	if (node.cnt)
	{
	    if (Beep_On)
		BEEP;   /* Beep for at least one node */
	    fflush(stdout);
	    sprintf(buffer, "NOTE: %d new nodes needed", node.cnt);
	    Write_info(3, buffer);
	    if(node.cnt == 2)     /* Beep for second node */
	    {
		sleep(1);
		if (Beep_On)
		    BEEP;
	    }
	}
	else
	{
	    Write_info (3, "NOTE: Zero new nodes needed");
	}

	if (do_graphics())
	    highlight_line (type, &Xpoints, 0, NULL);

	{
	  char *str;

	  if (type == DOT)
	      str = "Do you accept this site? ";
	  else
	      str = "Do you accept this line? ";

	  if (Digtiz_Device == MOUSE)
	      yes_no = mouse_yes_no(str) ;
	  else
	      yes_no = ask_yes_no(str) ;
	}

	if (yes_no)
	{
	    if (do_graphics ())
		erase_line (type, &Xpoints, 0, NULL);

	    Changes_Made = 1;
	    line = new_line (map, type, &node, &Xpoints);
	    if (line < 0)
	    {
		BEEP;
		Write_info (2, "Error creating new line.");
		sleep (4);
		return (-1);
	    }
	    if (do_graphics())
		display_line (type, &Xpoints, line, map);

	    /* is this an area boundary that will affect neighbor areas? */
	    if (type == AREA)
	    {
		if (area = check_next (map, line, RIGHT))
		    Del_area (map, area);
		if (area = check_next (map, line, LEFT))
		    Del_area (map, area);
		if (area = check_next (map, -line, RIGHT))
		    Del_area (map, area);
		if (area = check_next (map, -line, LEFT))
		    Del_area (map, area);
	    }

	    if (auto_label)
	    {
		label_line (map, line, auto_label, &Xpoints);
	    }

	}
	else
	{
	    if (do_graphics())
		erase_line (type, &Xpoints, 0, NULL);
	}
d_done:
	Clear_info();
	/*  if they don't have buttons force them to start over */
	if (Digtiz_Device != MOUSE)
	    if ( ! D_cursor_buttons())
		return(0) ;

    }	/*  while(1)  */

    /*NOTREACHED*/

}	/*  do_digitize()  */

