#include <stdlib.h>
#include <math.h>
#include "Vect.h"

static double dist_squared (double, double, double, double);

/* dig_node_add_line ()
**   add 'line' info to 'node'
**   line will be negative if END node 
**   'node' must of course already exist
**   space will be alloced to add 'line' to array
**
**   Returns -1 on error      
**            0 line not added  (degenerate)
**            else new number of lines in node 
**        
*/
int 
dig_node_add_line ( struct Plus_head *plus, int nodeid, int lineid,
		    struct line_pnts *points, int type)
{
    register int i, j, nlines;
    float angle;
    int end, ret;
    P_NODE *node;
    char *p;

    G_debug (3, "dig_node_add_line(): node = %d line = %d", nodeid, lineid);

    node = plus->Node[nodeid];
    nlines = node->n_lines;

    /* reallocate memory */
    ret = dig_node_alloc_line ( node, 1);
    if ( ret == -1 ) return -1;
    
    //end = line < 0 ? points->n_points - 1 : 0;

    if ( type & GV_LINES ) { 
	if (lineid < 0)
	{
	  //angle = dig_calc_end_angle (points, map->head.map_thresh);
	  angle = dig_calc_end_angle (points, 0);
	}
	else
	{
	  //angle = dig_calc_begin_angle (points, map->head.map_thresh);
	  angle = dig_calc_begin_angle (points, 0);
	}
    } else {
	angle = 9.;
    }
	
    /* make sure the new angle is less than the empty space at end */
    node->angles[nlines] = 999.;

    for (i = 0; i <= nlines; i++)	/* alloced for 1 more */
    {
      if (angle < node->angles[i])
	{
	  /* make room for insertion */
	  for (j = nlines - 1; j >= i; j--)
	    {
	      node->angles[j + 1] = node->angles[j];
	      node->lines[j + 1] = node->lines[j];
	    }
	  node->angles[i] = angle;
	  node->lines[i] = lineid;
	  break;
	}
    }

    node->n_lines++;
#ifdef GDEBUG
    G_debug (3, "dig_node_add_line(): line %d added position %d n_lines: %d angle %f", lineid, i, node->n_lines, angle);
#endif
    return ((int) node->n_lines);
}


/* dig_add_node ()
** add new node to plus structure 
**
** Returns -1 on error      
**          number of node
*/
int 
dig_add_node ( struct Plus_head *plus, double x, double y, double z) {
    int  nnum;
    char *p;
    P_NODE *node;
    
    /* First look if we have space in array of pointers to nodes
    *  and reallocate if necessary */
    G_debug(3, "dig_add_node(): n_nodes = %d, alloc_nodes = %d", plus->n_nodes, plus->alloc_nodes );
    if ( plus->n_nodes >= plus->alloc_nodes ) { /* array is full */
	if ( dig_alloc_nodes(plus,1000) == -1 )
            return -1;
    }
    
    /* allocate node structure */
    nnum = plus->n_nodes + 1;

    plus->Node[nnum] = dig_alloc_node();
      
    node = plus->Node[nnum];
    node->x = x;
    node->y = y;
    node->z = z;
 
    dig_spidx_add_node (plus, nnum, x, y, z); 
    
    plus->n_nodes++;
    
    G_debug(3, "new node = %d, n_nodes = %d, alloc_nodes = %d", nnum, plus->n_nodes, plus->alloc_nodes );
     
    return ( nnum );
}

/*  which_node returns the actual index into node arrays of the first set of
   *    matching coordinates.  else -1;
 */

int 
dig_which_node ( struct Plus_head *plus, double x, double y, double thresh) {
  register int i;
  register int first_time;
  register int have_match;
  int winner;
  double least_dist, dist;
  P_NODE *node;

  first_time = 1;
  have_match = 0;
  winner = 0;
  least_dist = 0.0;
  for (i = 1; i <= plus->n_nodes; i++)
    {
      if (plus->Node[i] == NULL)
	continue;

      node = plus->Node[i];
      if ((fabs (node->x - x) <= thresh) &&
	  (fabs (node->y - y) <= thresh))
	{
	  dist = dist_squared (x, y, node->x, node->y);
	  if (first_time)
	    {
	      least_dist = dist;
	      first_time = 0;
	      winner = i;
	      have_match = 1;
	    }
	  if (dist < least_dist)
	    {
	      least_dist = dist;
	      winner = i;
	    }
	}
    }

  if (!have_match)
    return (-1);

  return (winner);
}				/*  which_node ()  */


static double 
dist_squared (double x1, double y1, double x2, double y2)
{
  double dx, dy;

  dx = x1 - x2;
  dy = y1 - y2;
  return (dx * dx + dy * dy);
}
