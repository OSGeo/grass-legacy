/*  @(#)check_node.c    2.1  6/26/87  */
#include <math.h>
#include "Vect.h"

/*
   **  Last Modification: Dave Gerdes 5 1988
   **  US Army Construction Engineering Research Lab
 */

/*  file contains utility functions to access the x,y coordinate []'s  */
/* check_nodes()
   **   returns number of new nodes needed
   **   fills 'map'  with node indexes for N1 and N2  and cnt == new nodes needed.
   **   Calls which_node()
   **
   ** Index to new nodes are vapor, until the nodes are actually created with
   **   new_node ()
 */

static double dist_squared (double, double, double, double);

int 
dig_check_nodes (struct Map_info *map, struct new_node *node,
		 struct line_pnts *points)
{
  int new_nodes;
  int new_node;
  int last_node;

  int n_points;
  double *xarray;
  double *yarray;

  xarray = points->x;
  yarray = points->y;
  n_points = points->n_points;

  new_nodes = 0;
  last_node = map->n_nodes;

  new_node = dig_which_node (map, xarray, yarray, map->snap_thresh);
#ifdef DEBUG
  if (new_node == 0)
    debugf ("ERROR: dig_which_node returned 0  num_nodes: %d\n", map->n_nodes);
#endif
  if (new_node < 0)
    {
#ifdef DEBUG
      debugf ("Adding node: %d\n", last_node + 1);
#endif
      node->N1 = ++last_node;
      new_nodes++;
    }
  else
    node->N1 = new_node;

  /* see if this line has 2 points that snap */
  if ((fabs (xarray[0] - xarray[n_points - 1]) <= map->snap_thresh) &&
      (fabs (yarray[0] - yarray[n_points - 1]) <= map->snap_thresh))
    node->N2 = node->N1;
  else
    {

      new_node = dig_which_node (map, &(xarray[n_points - 1]), &(yarray[n_points - 1]), map->snap_thresh);
#ifdef DEBUG
      if (new_node == 0)
	debugf ("ERROR: dig_which_node returned 0  num_nodes: %d\n", map->n_nodes);
#endif
      if (new_node < 0)
	{
#ifdef DEBUG
	  debugf ("Adding node: %d\n", last_node + 1);
#endif
	  node->N2 = ++last_node;
	  new_nodes++;
	}
      else
	node->N2 = new_node;
    }

  node->cnt = new_nodes;

  return (new_nodes);
}


/*  which_node returns the actual index into node arrays of the first set of
   *    matching coordinates.  else -1;
 */

int 
dig_which_node (
		 struct Map_info *map,
		 double *x, double *y,
		 double thresh)
{
  register int i;
  register int first_time;
  register int have_match;
  int winner;
  double least_dist, dist;


  first_time = 1;
  have_match = 0;
  winner = 0;
  least_dist = 0.0;
/*
   #ifdef TESTING
 */
  for (i = 1; i <= map->n_nodes; i++)
    {
      if (!map->Node[i].alive)
	continue;

      if ((fabs (map->Node[i].x - *x) <= thresh) &&
	  (fabs (map->Node[i].y - *y) <= thresh))
	{
	  dist = dist_squared (*x, *y, map->Node[i].x, map->Node[i].y);
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
/*
   #endif
 */

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
