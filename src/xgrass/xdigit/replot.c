/*
**  Written by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
*/

#include "digit.h"
#include "wind.h"
#include "color.h"

void
redraw()
{
    clear_window();
    replot(CM);
    zero_display_list();
}

replot(map)
    struct Map_info *map;
{
    int x, y, w, h;
   TimeOutCursor (1);

    if (Disp_backdrop ? (display_backdrop () >= 0) : 1) /* backdrop first */ 
     if (Disp_overlay ? (display_overlay () >= 0) : 1)   /*then underlay file */
      if (display_all_lines  (map) >= 0)   		/* the work horse */
       if ((Disp_markers && !Disp_labels) ? (display_cents (map) >= 0) : 1) 
        if (Disp_labels ? (display_alabels (map) >= 0) : 1)
         if (Disp_outline ? (display_labeled_areas (map) >= 0) : 1) 
         
   redisplay_current_edit();
   clear();
   TimeOutCursor (0);
   write_type_info();
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
	    standard_color (dcolors[dig_node_color (Node[i].n_lines)]);
	    _Blot (&(Node[i].x), &(Node[i].y));
	}
}

/* returns 0 on completion,  or -1 if interupted by key press */
display_all_lines (map)
    struct Map_info *map;
{
    register int i;
    char buf[10];
    int ret;

    ret = 0;
if (!Disp_lines && !Disp_llines && !Disp_llabels && !Disp_points && !Disp_nodes && !Disp_sites && !Disp_slabels)
	return (1);
    for (i = 1 ; i <= map->n_lines ; i++)
    {
	   if (Check_for_interrupt())
	   {
		  ret = -1;
		     break;
	   }

	if (LINE_ALIVE (&(map->Line[i])) &&  line_in_window (&(map->Line[i]))) 
	{
	    V1_read_line (map, &Gpoints, map->Line[i].offset);
	    _display_line (map->Line[i].type, &Gpoints, i, map);
	}
    }
    return (ret);
}
