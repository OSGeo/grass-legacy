/*
*
*  snapping to the closest node and that node should
*  have already been snapped to the nodes around it.

*  return -  (node number) it should snap to
*  return -  (-1) if nothing to snap to.  (new node)
*
*/


#include <stdio.h>
#include <math.h>
#include "Vect.h"

int find_nodes (double x, double y, int n_nodes, struct P_node *NODES, double s_thresh)
{
	int i ;

	int close_node ;

	double x_dist, y_dist ;
	double x_small_dist, y_small_dist ;

	if( ! n_nodes)
		return(-1) ;

    /*  initiliaze it to something  */
	close_node = -1 ;
	x_small_dist =  9999999.0 ;
	y_small_dist =  9999999.0 ;

	for (i=0; i<n_nodes; i++)
	{
		x_dist = fabs(x - NODES[i].x) ;
		y_dist = fabs(y - NODES[i].y) ;

	/* find all other nodes within snapping thresh distance */
		if ( x_dist < s_thresh  &&  y_dist < s_thresh)
			if ( x_dist < x_small_dist  &&  y_dist < y_small_dist)
			{
				x_small_dist = x_dist ;
				y_small_dist = y_dist ;
				close_node = i ;
			}
			
	}	/*  for (i<n_nodes)  */

	return(close_node) ;
}
