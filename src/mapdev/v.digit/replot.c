/*
**  Written by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
*/

#include "digit.h"
#include "raster.h"
#include "wind.h"
#include "display_line.h"
#include "dig_curses.h"
#include "keyboard.h"
#include "Map_proto.h"
#include "local_proto.h"

int replot (struct Map_info *map)
{
    if (!do_graphics()) return (-1);
    Clear_info ();
    Write_info (2, "Wait. Replotting the Screen.");
    
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
     if (Disp_overlay ? (display_overlay () >= 0) : 1)  /* then underlay file */
      if (display_all_lines  (map) >= 0)   		/* the work horse */
       if ((Disp_markers && !Disp_labels) ? (display_cents (map) >= 0) : 1)
        if (Disp_labels ? (display_alabels (map) >= 0) : 1)
         if (Disp_outline ? (display_labeled_areas (map) >= 0) : 1)
		;
          add_scale () ; V_flush ();  /*OHLER*/
#endif

    V_flush ();
    Write_info (2, "");
	return(0) ;
}


int line_in_window (P_LINE *Line)
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
int _line_in_window ( P_LINE *Line,
    double north,double south,double east,double west)
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
int _line_really_in_window (
    struct Map_info *map,
    P_LINE *Line,
    double north,double south,double east,double west)
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

int display_nodes (struct Map_info *map)
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
    V_flush ();
	return(0) ;
}

/* returns 0 on completion,  or -1 if interupted by key press */
int display_all_lines (struct Map_info *map)
{
    register int i;
    char buf[10];
    int ret;

    ret = 0;
    Write_info (3, "");

if (!Disp_lines && !Disp_llines && !Disp_llabels && !Disp_ulines && !Disp_points && !Disp_nodes && !Disp_sites && !Disp_slabels)
	return (1);
    set_keyboard ();
    for (i = 1 ; i <= map->n_lines ; i++)
    {
	if (key_hit (buf))
	{
	    if (*buf == ESC)
	    {
		ret = -1;
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
    V_flush ();
    Write_info (3, "                                                                    ");
    return (ret);
}

#ifdef FOO
int 
replot_lines (struct Map_info *map, int points)
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
    V_flush ();
	return(0) ;
}
#endif

int 
display_tlines (struct Map_info *map, int type)
{
    register int i;

    for (i = 1 ; i <= map->n_lines ; i++)
    {
	if (LINE_ALIVE (&(map->Line[i])) && (map->Line[i].type & type) &&
		line_in_window (&(map->Line[i]))) 
	{
	    V1_read_line (map, &Gpoints, map->Line[i].offset);
	    display_line (map->Line[i].type, &Gpoints, i, map);
	}
    }
    V_flush ();
	return(0) ;
}
