/*  @(#)node_op.c    2.1  6/26/87  */
/*
   **  Written by: Dave Gerdes 5 1988
   **  US Army Construction Engineering Research Lab
 */


#include <stdlib.h>
#include "Vect.h"

/**  file contains functions that do a specific operation or access the x,y
*    coordinate arrays.
*    snap_line_to_node(), within_a_thresh(),
*    pr_nodes().
**/

/*  Snap_line_to_node ()
   **      
   **     line will be negative if we are looking at the end node
 */

int 
dig_snap_line_to_node (
			struct Map_info *map,
			int line, int node,
			struct line_pnts *points)
{
  int whichnode;
  P_LINE *MLN;

  /* remove line from old node */
  whichnode = line < 0 ? map->Line[abs (line)].N2 : map->Line[line].N1;
  dig_node_del_line (&(map->Node[whichnode]), line);

  MLN = &(map->Line[abs (line)]);

  if (line < 0)
    MLN->N2 = node;
  else
    MLN->N1 = node;

  if (0 > dig_node_add_line (map, &(map->Node[node]), line, points, 0))
    return (-1);

  dig_bound_box2 (points, &(MLN->N), &(MLN->S), &(MLN->E), &(MLN->W), map->head.orig_scale);	/*4.0 */

  return (0);
}

/* node_del_line ()
   **   removes any reference to 'line' in P_NODE struct for 'node'.
   **   also removes node if there was only one line.
   **   
   **   It does NOT remove node information in P_LINE struct for 'line'
   **
   **   Returns  -1 if line not connected to node else Number of lines left
   **    connected to node
   **
 */
int 
dig_node_del_line (
		    P_NODE * node,
		    int line)
{
  register int i, lines;

  line = abs (line);
  lines = node->n_lines;

/*      taken care of by the next test  
   if (lines == 0)
   return (-1);
 */

  for (i = 0; i < lines; i++)
    if (abs (node->lines[i]) == line)
      break;
  /* line is not connected to node */
  if (i > lines)
    return (-1);

  /* kill node if that was the only line */
  if (lines == 1)
    {
      node->alive = 0;
      if (node->alloc_lines)
	free (node->lines);
      node->alloc_lines = 0;
      node->n_lines = 1;	/* this will be decremented below */
    }

  /* scoot the rest up in place */
  for (++i; i < lines; i++)
    {
      node->lines[i - 1] = node->lines[i];
      node->angles[i - 1] = node->angles[i];
    }
  /* decrement line count */
  node->n_lines--;
  return ((int) node->n_lines);
}

/* node_add_line ()
   **    line will be negative if END node 
   **    add 'line' info to 'node'
   **    'node' must of course already exist
   **    space will be alloced to add 'line' to array
   **
   **   Returns -1 on error      
   **            0 line not added  (degenerate)
   **            else new number of lines in node 
   **        
 */
int 
dig_node_add_line (
		    struct Map_info *map,
		    P_NODE * node,
		    int line,
		    struct line_pnts *points,
		    int type)	/* this is a psuedo type.  only checked for == DOT */
{
  register int i, j, lines;
  float angle;
  int end_point;

  lines = node->n_lines;

  if (0 > dig_node_alloc_line (node, 1))
    {
      return (-1);
    }

  end_point = line < 0 ? points->n_points - 1 : 0;
  if (!node->alive)
    {
      node->x = points->x[end_point];
      node->y = points->y[end_point];
    }
  else
    {
      points->x[end_point] = node->x;
      points->y[end_point] = node->y;
    }

  if (line < 0)
    {
      /* I put these back to head instead of map, because
         **  build zeroes map->snap_thresh if it does not
         **  want snapping.  BUT we still want to figure
         **  angles with the same method.
       */
      /*
         angle = dig_calc_end_angle (points, map->snap_thresh);
       */
      angle = dig_calc_end_angle (points, map->head.map_thresh);
    }
  else
    {
      /*  
         angle = dig_calc_begin_angle (points, map->snap_thresh);
       */
      angle = dig_calc_begin_angle (points, map->head.map_thresh);
    }

  if (angle == -9. && type != DOT)
    {
#ifdef DEBUG
      fprintf (stderr, "NODE_ADD_LINE: Line %d  is degenerate. skipping\n", line);
#endif
      return (0);
    }

  /* make sure the new angle is less than the empty space at end */
  node->angles[lines] = 999.;

  for (i = 0; i <= lines; i++)	/* alloced for 1 more */
    {
      if (angle < node->angles[i])
	{
	  /* make room for insertion */
	  for (j = lines - 1; j >= i; j--)
	    {
	      node->angles[j + 1] = node->angles[j];
	      node->lines[j + 1] = node->lines[j];
	    }
	  node->angles[i] = angle;
	  node->lines[i] = line;
	  break;
	}
    }

  node->alive = 1;
  node->n_lines++;
#ifdef DEBUG
  debugf ("node_add_line: line %d added position %d n_lines: %d angle %f\n", line, i, node->n_lines, angle);
#endif
  return ((int) node->n_lines);
}


/* add a new node at postion num in map->Node array.  space is allocated */
/* if needed */

/* num is new node number 
   ** line is line number we are dealing w/   negative if this is the END node
   ** map is pointer to Map_info struct 
   ** points holds the line point info
   ** 
   **  allocates room for new node (if needed) and then
   ** calls node_add_line() to do the dirty work
   **   returns -1 error
   **         0 line not added  (degenerate)
   **         node number if line added
 */
int 
dig_add_line_to_node (
		       int num, int line,
		       char type,
		       struct Map_info *map,
		       struct line_pnts *points)
{
  int ret;

  if (num > map->n_nodes)
    {
      if (0 > dig_alloc_node (map, 1))
	{
	  return (-1);
	}
      /* make sure alloc_lines is  0 */
      map->Node[num].alloc_lines = 0;
      map->Node[num].n_lines = 0;
      map->Node[num].alive = 0;
    }

/* now fill in line and angle info */
  if (0 > (ret = dig_node_add_line (map, &(map->Node[num]), line, points, type)))
    {
      return (-1);
    }
  if (ret == 0)			/* degenerate line */
    return (0);

  /* this must stay */
  map->Node[num].alive = 1;
  return (num);
}
