/*
**  Last modified by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
*/

#include "digit.h"
#include <math.h>

#define	YES    1
#define	NO    2
#define	QUIT    3

double hypot ();
double fabs();
long ftell();

snap_nodes(map)
    struct Map_info *map;
{
    double thresh;
    double ux1, uy1;
    double ux2, uy2;
    double hyp1, hyp2;
    int end_point;
    int area;
    int line, line_from, node_num;

    screen_to_utm (0, 0, &ux1, &uy1);
    screen_to_utm (5, 0, &ux2, &uy2);
    thresh = fabs ( ux1 - ux2);

    while (1)
    {
	Clear_info ();
	/* find_line_with_mouse  fills Gpoints */
	line_from = find_line_with_mouse (LINE|AREA, " Line to snap  FROM:", NULL);
	line = line_from;

	if (line_from <= 0)
	    return (0);
	node_num = find_node_with_mouse ( &ux2, &uy2, thresh, " Node to snap  TO:");
	if ( node_num <= 0)
	{
	    /* reset the highlit line */
	    display_line (map->Line[line].type, &Gpoints, line, map);
	    return (0);
	}

	    Write_info ( 2, "");

	/* if node is on same line, warn user */
	if (map->Line[line_from].N1 == node_num || map->Line[line_from].N2 == node_num)
	if (!curses_yes_no_default (2, "Attempting to snap line to itself. Proceed? ", 1))
	    continue;
    /* at this point Gpoints holds line point information from 
    **   find_line_with_mouse()
    */
	{
	    Changes_Made = 1;
	    erase_line( map->Line[line].type, &Gpoints, line, map);

	    end_point = Gpoints.n_points-1;
	    /* snap to closest node on 'line_from' */

	    {
		register double X, Y;

		X = Gpoints.x[0] - map->Node[node_num].x; 
		Y = Gpoints.y[0] - map->Node[node_num].y;
		if (X == 0.0 && Y == 0.0)
		    hyp1 = 0.0;
		else
		    hyp1 = hypot (X, Y);
		/*
		if (near_zero (hyp1))
		    hyp1 = 0.0;
		*/

		X = Gpoints.x[end_point] - map->Node[node_num].x; 
		Y = Gpoints.y[end_point] - map->Node[node_num].y;
		if (X == 0.0 && Y == 0.0)
		    hyp2 = 0.0;
		else
		    hyp2 = hypot (X, Y);
		/*
		if (near_zero (hyp2))
		    hyp2 = 0.0;
		*/
	    }


    /* choose which end point to snap from:  
    **       Do not want to snap to itself so check hyp for 0.0 
    **	 otherwise  endpoint closest to node gets snapped
    */
	    if (hyp1 == 0.0)
		line_from = -line_from;
	    else
		if (hyp2 == 0.0)
		    ;  /* leave positive */
		else
		    if (hyp1 > hyp2)
			line_from = -line_from;
		    else
			;

	    /* delete areas bounded by line  because areas have changed */
	    /* ISLE
	    **   Del_area will call Del_isle automatically
	    */
	    if (map->Line[line].type == AREA)
	    {
		if (map->Line[line].right)
		    Del_area (map, map->Line[line].right);
		if (map->Line[line].left)
		    Del_area (map, map->Line[line].left);
	    }

    /*DEBUG*/ debugf ("Calling Snap_line_to_node (line %d, node, %d)\n", line_from, node_num);
	    dig_snap_line_to_node (map, line_from, node_num, &Gpoints);

	    /* check for possibly affecting existing areas at the snapped node */
	    if (map->Line[line].type == AREA)
	    {
		if (area = check_next (map, line_from, RIGHT))
		    Del_area (map, area);
		if (area = check_next (map, line_from, LEFT))
		    Del_area (map, area);
	    }

	    display_line (map->Line[line].type, &Gpoints, line, map);

	    Vect__Rewrite_line (map, map->Line[line].offset, map->Line[line].type, &Gpoints);

	}
    }

    Clear_info();
}

near_zero (x)
    double x;
{
    return (fabs(x) < .000002 ? 1 : 0);
}
