/*
**  Written by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
*/

#include "digit.h"
#include "wind.h"
#include "popup.h"

replot(map)
    struct Map_info *map;
{
    char buf[50];

    if (!do_graphics()) return (-1);

    flush_keyboard (); 
#ifdef OLDWAY
    if (Disp_backdrop ? (display_backdrop () >= 0) : 1) /* backdrop first */
     if (display_all_lines  (map) >= 0)   		/* the work horse */
      if ((Disp_markers && !Disp_labels) ? (display_cents (map) >= 0) : 1)
       if (Disp_labels ? (display_alabels (map) >= 0) : 1)
        if (Disp_outline ? (display_labeled_areas (map) >= 0) : 1)
	 if (Disp_overlay) display_overlay ();
#else
    if (Disp_backdrop ? (display_backdrop () >= 0) : 1) /* backdrop first */
     if (Disp_overlay ? (display_overlay () >= 0) : 1) /* then underlay file */
      if (display_all_lines  (map) >= 0)   		/* the work horse */
       if ((Disp_markers && !Disp_labels) ? (display_cents (map) >= 0) : 1)
        if (Disp_labels ? (display_alabels (map) >= 0) : 1)
         if (Disp_outline ? (display_labeled_areas (map) >= 0) : 1)
		;
#endif

    R_flush ();
}


line_in_window (Line)
    P_LINE *Line;
{
    if (Line->S > U_north)
	return (0);
    if (Line->N < U_south)
	return (0);
    if (Line->W > U_east)
	return (0);
    if (Line->E < U_west)
	return (0);
    return (1);
}

/* quick test to see if line bbox is in window */
/*  1 yes 0 no */
_line_in_window (Line, north, south, east, west)
    P_LINE *Line;
    double north, south, east, west;
{
    if (Line->S > north)
	return (0);
    if (Line->N < south)
	return (0);
    if (Line->W > east)
	return (0);
    if (Line->E < west)
	return (0);
    return (1);
}

/* 
**  line_really_in_window  - check each point in line against window
**
*/
_line_really_in_window (map, Line, north, south, east, west)
    struct Map_info *map;
    P_LINE *Line;
    double north, south, east, west;
{
    register int i;

    if (0 > V1_read_line (map, &Gpoints, Line->offset))
	return (0);

    for (i = 0 ; i < Gpoints.n_points ; i++)
    {
	if (!(Gpoints.x[i] > west && Gpoints.x[i] < east &&
		Gpoints.y[i] < north && Gpoints.y[i] > south))
	    return (0);
    }
    return (1);
}

display_nodes (map)
    struct Map_info *map;
{
    register int i;
    P_NODE *Node;

    Node = map->Node;
    for (i = 1 ; i <= map->n_nodes ; i++)
	if (NODE_ALIVE (&(Node[i])))
	{
	    R_standard_color (dcolors[dig_node_color (Node[i].n_lines)]);
	    _Blot (&(Node[i].x), &(Node[i].y));
	}
    R_flush ();
}

/* returns 0 on completion,  or -1 if interupted by key press */
display_all_lines (map)
    struct Map_info *map;
{
    register int i;
    char buf[50];
    int ret;

    if (map->n_lines == 0) return (-1);

    ret = 0;
    sprintf(buf," Displaying Vector data."); 
    message[0] = (char *) malloc (strlen (buf) + 1);
    sprintf(message[0],"%s", buf);
    sprintf(buf,"...Press < ESC > key to stop redraw .");
    message[1] = (char *) malloc (strlen (buf) + 1);
    sprintf(message[1],"%s", buf);
    message[2] = '\0';

    Dchoose(MEN.name) ;
    popup_messg( "disp_vect", 1) ;

    if (!Disp_lines && !Disp_llines && !Disp_llabels && 
	!Disp_points && !Disp_nodes && !Disp_sites && 
	!Disp_slabels)
	 {
         erase_popup("disp_vect");
 	 return (1);
	 }

    Dchoose(DIG.name) ;
    set_keyboard ();		/* setup for kbhit () */
    for (i = 1 ; i <= map->n_lines ; i++)
    {
	if (key_hit (buf))
	{
	   if (*buf == ESC)
	   {
	   /* ret = -1; */
	      break;
	   }
        }

	if (LINE_ALIVE (&(map->Line[i])) &&  line_in_window (&(map->Line[i]))) 
	{
	    V1_read_line (map, &Gpoints, map->Line[i].offset);
	    _display_line (map->Line[i].type, &Gpoints, i, map);
	}
    }
    unset_keyboard ();
    R_flush ();
    erase_popup("disp_vect");
    return (ret);
}

#ifdef FOO
replot_lines (map, points)
    struct Map_info *map;
    int points;
{
    register int i;
    register P_LINE *Line;

    for (i = 1 ; i <= map->n_lines ; i++)
    {
	Line = &(map->Line[i]);
	if (LINE_ALIVE (Line) &&  line_in_window (Line)) 
	{
	    V1_read_line (map, &Gpoints, Line->offset);

	    /* this function now exists in display_line()
	    if (Disp_llines && Line->att && map->Att[Line->att].cat)
		color_line (Line->type, &Gpoints, i, map, CLR_LLINE);
	    else
		if (points)
	    */
		    display_line (Line->type, &Gpoints, i, map);
	    /*
		else
		    ndisplay_line (Line->type, &Gpoints, i, map);
	    */
	}
    }
    R_flush ();
}
#endif

display_tlines (map, type)
    struct Map_info *map;
    char type;
{
    register int i;

    for (i = 1 ; i <= map->n_lines ; i++)
    {
	if (LINE_ALIVE (&(map->Line[i])) && 
	      (map->Line[i].type & type) &&
	      line_in_window (&(map->Line[i]))) 
	{
	    V1_read_line (map, &Gpoints, map->Line[i].offset);
	    display_line (map->Line[i].type, &Gpoints, i, map);
	}
    }
    R_flush ();
}
