/*  @(#)digitize.c    2.1  6/26/87  */
/*
**  Last modified by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
*/

#include <stdio.h>
#include "gis.h"
#include "digit.h"
#include "dig_head.h"

#ifdef BEEP
#undef BEEP
#endif

#define BEEP        XBell (dpy,25)

/* made these global so modes would remain set if they leave and return */
static int    type = AREA;
static int    mode = POINT;
static int    auto_label = 0;


Digitize (w, command)
    Widget w;
    int    command;
{
    int ret;


    switch(command)
    {
	case MDC_MODE:
	    TOGGLE (mode);
	    modecb();
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
	    break;
	case MDC_LABEL:
	    ask_value ("Enter category number:", &auto_label);
	    if (!auto_label)
	        XtVaSetValues (w, XmNset, False, NULL);
	    break;
	case MDC_UNLABEL:
	    auto_label = 0;
	    break;
	case MDC_DIGIT:

	    do_digitize(CM, type == DOT ? POINT : mode, type, 
		CM->prune_thresh, 1, 0);
	    break;
	default:
	    break;
    }
}


do_digitize(map, mode, type, sample_thresh, multi, close_area)
    struct Map_info *map;
    int mode;
    int type;
    double sample_thresh;
    int multi;   /* for SCS PSU */
    int close_area;	/* snap last point to first for SCS */
{
    char buffer[64];
    int stream_mode;
    struct new_node node;
    int line;
    int area;
    int yes_no ;
    static struct line_pnts Xpoints;
    static int first = 1;
/*DEBUG*/ debugf ("do_digitize\n");


    if (first)
    {
	first = 0;
	Xpoints.alloc_points = 0;
	Xpoints.n_points = 0;
    }
    if (Digtiz_Device == MOUSE && type == DOT)
        show_select_dialog (NULL, "abort", "Digitize a point", 0) ;

    while(1)
    {
	/* if DOT, can use STOP digitizing to quit 
	*/
	if (type != DOT)
	{
	    if (Digtiz_Device != MOUSE)
#ifdef CURSORKEYS
	    if (D_cursor_buttons())
#endif
	    {
			if ( ! ask_driver_yes_no("Begin digitizing? ") )
			    return(0) ;
	    }
	}
	else
        {
	    write_info(1, "Site digitizing") ;
        } 

	if (Digtiz_Device == MOUSE)
	    stream_mode = mouse_collect_points(mode, (char) type, &Xpoints);
	else
	{
	    show_dig_menu(); 
	    stream_mode = Collect_points(mode, (char) type, &Xpoints);
	    close_dig_menu();
	}
	if (close_area)
	{
      /* match the first and last points to snap */
	    dig_alloc_points (&Xpoints, Xpoints.n_points+1);
	    Xpoints.x[Xpoints.n_points] = Xpoints.x[0];
	    Xpoints.y[Xpoints.n_points] = Xpoints.y[0];
	    Xpoints.n_points++;
	}

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
	  if (!close_area)	/* SCS*/
	  {
	    if (Xpoints.n_points == 1 ||  /* are all points w/in snapping thresh? */
		 dig_is_line_degenerate (&Xpoints, map->head.map_thresh))
	    {
		Xpoints.n_points = 0;
		BEEP;
		
	  	if (Digtiz_Device == MOUSE)
		    yes_no = mouse_yes_no
		      ("Only 1 point digitized, Ignoring... Begin digitizing?");
		else
		    yes_no = ask_driver_yes_no
		      ("Only 1 point digitized, Ignoring... Begin digitizing?");
		if (yes_no)
		    continue;
		break;
	    }
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
	{
/*DEBUG*/  debugf (" Entering Checknodes HAVE %d Points.\n", Xpoints.n_points);
	    dig_check_nodes (map, &node, &Xpoints);
/*DEBUG*/ debugf ( "Check_nodes returns  N1 = %d  N2 = %d\n", node.N1, node.N2);
	}
	if (node.cnt)
	{
	    if (Beep_On)
		BEEP;   /* Beep for at least one node */
	    sprintf(buffer, "NOTE: %d new nodes needed", node.cnt);
	    write_info(1, buffer);
	    if(node.cnt == 2)     /* Beep for second node */
	    {
		sleep(1);
		if (Beep_On)
		    BEEP;
	    }
	    XFlush (dpy);
	}
	else
	{
	    write_info (1, "NOTE: Zero new nodes needed");
	}

	highlight_line ((unsigned char) type, &Xpoints, 0, NULL);
	{
	  char *str;

	  switch (type) {
	      case DOT:
		  str = "Do you accept this site? ";
		  break;
	      case AREA:
		  if (close_area)
		      str = "Do you accept this area? ";
		  else
		      str = "Do you accept this area line? ";
		  break;
	      case LINE:
	      default:
		  str = "Do you accept this line? ";
		  break;
	  }

	  if (Digtiz_Device == MOUSE)
	      yes_no = mouse_yes_no(str) ;
	  else
	      yes_no = ask_driver_yes_no(str) ;
	}

	if (yes_no)
	{
		erase_line ((unsigned char)type, &Xpoints, 0, NULL);

	    Changes_Made = 1;
/*DEBUG*/ debugf ("entering new_line");
	    line = new_line (map, (unsigned char) type, &node, &Xpoints);
	    if (line < 0)
	    {
		BEEP;
		write_info(1,"Error creating new line.");
		return (-1);
	    }
	    display_line ((unsigned char) type, &Xpoints, line, map);

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
		/*
		** PSU, dont want digizing loop
		*/
                if (!multi) return(1); /* SCS_MODS */
	    }

	    if (auto_label)
	    {
		label_line (map, line, auto_label, &Xpoints);
                if (!multi) return(1); /* SCS_MODS */
	    }

	}
	else
	{
		erase_line ((unsigned char) type, &Xpoints, 0, NULL);
	}
        write_type_info();
d_done:
      ;
#ifdef CURSORKEYS
	/*  if they don't have buttons force them to start over */
	if (Digtiz_Device != MOUSE)
	    if ( ! D_cursor_buttons())
		return(0) ;
#endif
    }	/*  while(1)  */

    /*NOTREACHED*/

}	/*  do_digitize()  */

