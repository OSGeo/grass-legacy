#include "Vect.h"

/* make new line.  write line info to digit file, create new node info
**   and fill in P_LINE info
**
** 'node' contains N1, N2 indexes. (if they are new, they dont yet exist)
**    and number of new nodes info.
**
**  if endpoints have been snapped: update that in the digit file
**
**  returns   -1 on error   or  new line number
*/
int 
import_line (struct Map_info *map, int type, struct new_node *node, struct line_pnts *points, long offset)
{
    int num;
    int line;
    P_LINE *MLN;	/*  &(map->Node[num]) */

    line = map->n_lines + 1;	/* starts at 1 */

    if (0 > dig_alloc_line (map, 1))
    {
	dig_out_of_memory ();
	exit (-1);
    }

    /* make line negative for tail endpoint */
    if (0 > (num = dig_add_line_to_node ((int) node->N1, line, type, map, points)))
    {
	dig_out_of_memory ();
	exit (-1);
    }
    if (num > map->n_nodes)
	map->n_nodes = num;

    if (0 > (num = dig_add_line_to_node ((int) node->N2, -line, type, map, points))) 
    {
	dig_out_of_memory ();
	exit (-1);
    }
    if (num > map->n_nodes)
	map->n_nodes = num;

    /* replaced by the 2 conditionals above 
    map->n_nodes += node->cnt;
    */

    MLN = &(map->Line[line]);
    MLN->N1 = node->N1;
    MLN->N2 = node->N2;
    dig_bound_box2 (points, &(MLN->N), &(MLN->S), &(MLN->E), &(MLN->W), map->head.orig_scale); /* 4.0 */

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

    return (line);
}
