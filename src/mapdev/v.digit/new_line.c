/*
**  Written by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
*/

#include "digit.h"
#include "debug.h"
#include "Map_proto.h"
#include "local_proto.h"

/* make new line.  write line info to digit file, create new node info
**   and fill in P_LINE info
**
** 'node' contains N1, N2 indexes. (if they are new, they dont yet exist)
**    and number of new nodes info.
**
**  returns   -1 on error   or  new line number
*/
int 
new_line (struct Map_info *map, int type, struct new_node *node, struct line_pnts *points)
{
    int line;
    int num;
    long offset;
    P_LINE *MLN;	/*  &(map->Node[num]) */
    char *p;

    line = map->n_lines + 1;	/* starts at 1 */

    if (0 > dig_alloc_line (map, 1))
	return (-1);

    /* make line negative for tail endpoint */
    /* TODO   WHY? */
    /*
    if (type != DOT)
    */
    {
	/* make line negative for tail endpoint */
/*DEBUG*/ debugf ("Calling dig_and_line_to_node (Node %d, Line %d, Type %d, nPoints %d\n", node->N1, line, type, points->n_points);
	if (0 > (num = dig_add_line_to_node ((int) node->N1, line, type, map, points)))
	{
/*DEBUG*/ debugf ("failed\n");
	    return (-1);
	}
/*DEBUG*/ debugf ("returned\n");
	if (num > map->n_nodes)
	    map->n_nodes = num;

/*DEBUG*/ debugf ("Calling dig_and_line_to_node (Node %d, Line %d, Type %d, nPoints %d\n", node->N2, -line, type, points->n_points);
	if (0 > (num = dig_add_line_to_node ((int) node->N2, -line, type, map, points))) 
	{
/*DEBUG*/ debugf ("failed\n");
	    return (-1);
	}
/*DEBUG*/ debugf ("returned\n");
	if (num > map->n_nodes)
	    map->n_nodes = num;
    }

    MLN = &(map->Line[line]);
    MLN->N1 = node->N1;
    MLN->N2 = node->N2;
    dig_bound_box2 (points, &(MLN->N), &(MLN->S), &(MLN->E), &(MLN->W), map->head.orig_scale); /*4.0*/

/*DEBUG*/ debugf ("calling Vect_write_line %d points\n", points->n_points);
    offset = Vect_write_line (map, type, points);
    MLN->offset = offset;
    MLN->att = 0;
    MLN->type = type;
    map->n_points += points->n_points;	/* update total count of points now */

    switch (type)
    {
	case AREA:
	    ++map->n_alines;
	    ++map->n_lines;
	    break;
	case LINE:
	    ++map->n_llines;
	    ++map->n_lines;
	    break;
	case DOT:
	    ++map->n_plines;
	    ++map->n_lines;
	    break;
	default:
	    break;
    }

/*DEBUG*/ debugf ("returning from new_line\n");
    return (line);
}
