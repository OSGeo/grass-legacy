/*  @(#)snap.c    2.1  6/26/87  */
#include <math.h>
#include "digit.h"
#include "debug.h"
#include "raster.h"
#include "display_line.h"
#include "dig_curses.h"
#include "Map_proto.h"
#include "local_proto.h"

#define	FRONT    0
#define	BACK    1

#define	YES    1
#define	NO    2
#define	QUIT    3

/* THIS FILE HAS TO BE UPDATED YET */
int move_point (struct Map_info *map)
{
    double thresh ;
    double ux1, uy1 ;
    double ux2, uy2 ;
    int i ;
    int point_num ;
    int ret;
    int node;
    int line;
    int Save_Disp;


    screen_to_utm (0, 0, &ux1, &uy1) ;
    screen_to_utm (5, 0, &ux2, &uy2) ;
    thresh = fabs ( ux1 - ux2);

    Save_Disp = Disp_points;
    Disp_points = 1;

    line = find_line_with_mouse (LINE|AREA, "Choose line to modify:", NULL);
    if (line <= 0)
    {
	ret = 0;
	goto done;
    }

/*  find which point in line to move  */
    point_num = find_point_with_mouse(Gpoints.x, Gpoints.y,Gpoints.n_points,
	thresh) ;
    if (point_num < 0)
    {
	ret = 0;
	display_line( map->Line[line].type, &Gpoints, line, map);
	goto done;
    }

/*  find where to place the point   */
    new_point_with_mouse ( &ux1, &uy1,
	"  Place mouse on new position for point:");
    if( ! ux1 && !uy1)
    {
	ret = 0;
	display_line( map->Line[line].type, &Gpoints, line, map);
	goto done;
    }

    R_standard_color (dcolors[CLR_ERASE]);
    Blot (&Gpoints.x[point_num], &Gpoints.y[point_num]);
    erase_line( map->Line[line].type, &Gpoints, line, map);
/*DEBUG*/ debugf ("point_num = %d   n_points: %d\n", point_num, Gpoints.n_points);
    if (point_num == 0 || point_num == Gpoints.n_points-1)
    {
/*DEBUG*/ debugf ("Doing node on line\n");
	/* we have a node */
    {
	node = dig_which_node (map, &ux1, &uy1, map->snap_thresh);
	if (node > 0 )
	{
	    /* erase highlit locatio */
	    R_standard_color (dcolors[CLR_ERASE]);
	    Blot (&ux1, &uy1);

    /*DEBUG*/ debugf ("move_node: moving to existing node(%d)\n", node);
	    ux1 = map->Node[node].x;
	    uy1 = map->Node[node].y;
	}
	else
	{
	    /* Create a new node to snap line to */
	    if (0 > dig_alloc_node (map, 1))
	    {
	    ret = -1;
	    goto done;
	    }
	    node = ++(map->n_nodes);
    /*DEBUG*/ debugf ("move_node: creating a new node (%d)\n", node);
	    map->Node[node].x = ux1;
	    map->Node[node].y = uy1;
	    map->Node[node].n_lines = 0;
	    map->Node[node].alloc_lines = 0;

	    map->Node[node].alive = 1;
	}
    }
	Gpoints.x[point_num] = ux1;
	Gpoints.y[point_num] = uy1;

	if (point_num == 0)
	{
	    dig_snap_line_to_node(map,  line,	       node, &Gpoints);
	    dig_snap_line_to_node(map, -line, map->Line[line].N2, &Gpoints);
	}
	else
	{
	    dig_snap_line_to_node(map,  line, map->Line[line].N1, &Gpoints);
	    dig_snap_line_to_node(map, -line,	       node, &Gpoints);
	}

    }
    else
    {
	/* update point */
/*DEBUG*/ debugf ("Doing point inside line\n");
	Gpoints.x[point_num] = ux1;
	Gpoints.y[point_num] = uy1;


	/* and resnap line to recalc angles */
	dig_snap_line_to_node (map, line, map->Line[line].N1, &Gpoints);
	dig_snap_line_to_node (map, -line, map->Line[line].N2, &Gpoints);

    }
    Changes_Made = 1;
    display_line( map->Line[line].type, &Gpoints, line, map);

/*  Rewind, write out altered-code  */
    Vect__Rewrite_line (map, map->Line[line].offset, map->Line[line].type, &Gpoints);

    ret = 1;
done:
    Clear_info() ;
    Disp_points = Save_Disp;
    return (ret);
}
