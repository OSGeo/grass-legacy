/*
**  Written by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
*/
/* modified by RL Glenn  12/1991
** USDA, SCS, Tech. Infor. Sys. Division
*/


#include "digit.h"
#include "dig_head.h"

double fabs ();

/* ask user to choose a node  and then they can look at each
**  line attached to it, individually.   Great for finding double 
**  digitized lines
*/
node_lines (map)
    struct Map_info *map;
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
            sprintf (buf, "Node has NO lines attached to it .");
            message[0] = (char *) malloc (strlen (buf) + 1);
            sprintf(message[0],"%s", buf);
            message[1] = " ";
            message[2] = '\0';

            Dchoose(MEN.name) ;
            popup_messg( "info", 1) ;
	    sleep (2);
	    erase_popup("info");
            Dchoose(DIG.name) ;
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

	    rline_num = map->Node[node_num].lines[next_line];
	    line_num = ABS (rline_num);
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


	    sprintf(buf, "Node %3d  Line %3d  Type '%s'    ",
	      node_num, rline_num, tell_type(map->Line[line_num].type));
            message[0] = (char *) malloc (strlen (buf) + 1);
            sprintf(message[0],"%s", buf);
	    sprintf(buf, "          Angle  %7.5f (%7.5f) .", 
	        map->Node[node_num].angles[next_line], angle);
            message[1] = (char *) malloc (strlen (buf) + 1);
            sprintf(message[1],"%s", buf);
            message[2] = " ";
            message[3] = '\0';

            Dchoose(MEN.name) ;
	    if (!first) popup_messg( "info", 0) ;
	    else popup_messg( "info", 1) ;
	    first = 0;

	} while (2 != (Next = mouse_next_prev ("Select Next line:")));
	erase_popup("info");
	display_line(map->Line[line_num].type, &Gpoints, line_num, map);
    }
}

char *
tell_type (type)
    char type;
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
