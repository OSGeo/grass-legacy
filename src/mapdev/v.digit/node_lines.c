/*
**  Written by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
*/

#include <unistd.h>
#include <math.h>
#include "digit.h"
#include "raster.h"
#include "dig_curses.h"
#include "display_line.h"
#include "Map_proto.h"
#include "local_proto.h"

/* ask user to choose a node  and then they can look at each
**  line attached to it, individually.   Great for finding double 
**  digitized lines
*/
int 
node_lines (struct Map_info *map)
{
    register int Next, prev_line, next_line, line_num, node_num;
    int prev_node;
    int rline_num;
    char buf[1000];
    double ux1, uy1;
    double ux2, uy2;
    double thresh;
    float angle;
    int first;

    screen_to_utm (0, 0, &ux1, &uy1);
    screen_to_utm (5, 0, &ux2, &uy2);
    thresh = fabs ( ux1 - ux2);

    Clear_info();

    node_num = 0;
    while (1)
    {
	prev_node = node_num;
	node_num = find_node_with_mouse (&ux2, &uy2, thresh, "Select a Node:");
	if ( node_num <= 0)
	{
	    if (prev_node)
	    {
	    R_standard_color(dcolors[dig_node_color(map->Node[prev_node].n_lines)]);
	    Blot (&(map->Node[prev_node].x), &(map->Node[prev_node].y));
	    }
	    return (0);
	}
	if (map->Node[node_num].n_lines == 0)
	{
	    Write_info (2, "Node has NO lines attached to it.");
	    sleep (1);
	    Write_info (2, "");
	    continue;
	}
	next_line = map->Node[node_num].n_lines - 1;
	Next = 3;
	prev_line = 0;
	first = 1;
	do {
	    prev_line = next_line;
	    switch (Next) {
		case 1:		/* prev */
		    next_line = (next_line == 0 ? 
				map->Node[node_num].n_lines -1 : next_line-1);
		    break;
		case 2:		/* end */
		    /* shouldn't get here */
		    break;
		case 3:		/* next */
		    next_line = (next_line == map->Node[node_num].n_lines -1 ?
				    0 : next_line+1);
		    break;
	    }
	    if (!first)
	    {
		display_line(map->Line[line_num].type, &Gpoints, line_num, map);
	    }
	    first = 0;

	    rline_num = map->Node[node_num].lines[next_line];
	    line_num = abs (rline_num);
	    if (0 > V1_read_line (map, &Gpoints, map->Line[line_num].offset))
		continue;
	    highlight_line (map->Line[line_num].type, &Gpoints, line_num, map);

    /* calculate the angle on the fly for verification */
    if (rline_num < 0)
    {
	angle = dig_calc_end_angle (&Gpoints, map->head.map_thresh);
    }
    else
    {
	angle = dig_calc_begin_angle (&Gpoints, map->head.map_thresh);
    }


	    sprintf(buf, "Node %d  Line %d   Type '%s' Angle  %7.5f (%7.5f)\n", node_num, rline_num, 
		tell_type(map->Line[line_num].type),
	        map->Node[node_num].angles[next_line], angle);
	    Write_info (2, buf);

	} while (2 != (Next = mouse_next_prev ("Select Next line:")));
	display_line(map->Line[line_num].type, &Gpoints, line_num, map);
    }
}

char *
tell_type (int type)
{
    char *p;

    switch (type) {
	
	case LINE:
	    p = "Line";
	    break;
	case AREA:
	    p = "Area Border";
	    break;
	case DOT:
	    p = "Site Marker";
	    break;
	case DEAD_LINE:
	    p = "Deleted Line";
	    break;
	case DEAD_AREA:
	    p = "Deleted Area Border";
	    break;
	case DEAD_DOT:
	    p = "Deleted Site Marker";
	    break;
    }
    return (p);
}
