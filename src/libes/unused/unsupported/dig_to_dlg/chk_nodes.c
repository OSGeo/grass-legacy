/*  @(#)check_nodes.c	1.2  6/24/87  */
#include <math.h>
#include "structures.h"

check_nodes(n_points, xarray, yarray, thresh, beg_node, end_node)
	int n_points ;
	double *xarray ;
	double *yarray ;
	double thresh ;
	int *beg_node ;
	int *end_node ;
{
	double fabs() ;
	int at_node ;
	double line_beg_x ;
	double line_end_x ;
	double line_beg_y ;
	double line_end_y ;
	double node_x ;
	double node_y ;
	int samp_line ;
	int beg_match ;
	int end_match ;
#ifdef DEBUG
	char buffer[128] ;
#endif DEBUG

	line_beg_x = *xarray ;
	line_beg_y = *yarray ;
	line_end_x = xarray[n_points-1] ;
	line_end_y = yarray[n_points-1] ;
	beg_match = 0 ;
	end_match = 0 ;
	*beg_node = 0 ;
	*end_node = 0 ;

/* Search all nodes for matches */
	for(at_node=1; at_node<=n_nodes; at_node++)
	{
#ifdef DEBUG
		sprintf(buffer,"node %d", at_node) ;
		Write_info(3, buffer) ;
		sleep(2) ;
#endif DEBUG

		if (*beg_node && *end_node)
			break ;

#ifdef DEBUG
		if (! node_lines[at_node].n_lines)
		{
			Write_info(4, "No lines attached") ;
			sleep(2) ;
		}
#endif DEBUG

		if (! node_lines[at_node].n_lines)
			continue ;

		/* find coordinates for node */
		samp_line = node_lines[at_node].lines[0] ;
		if(samp_line > 0)
		{
			node_x = endpoints[lines[samp_line].endpoint_beg].x ;
			node_y = endpoints[lines[samp_line].endpoint_beg].y ;
		}
		else
		{
			node_x = endpoints[lines[-samp_line].endpoint_end].x ;
			node_y = endpoints[lines[-samp_line].endpoint_end].y ;
		}

#ifdef DEBUG
		sprintf(buffer, "Coors: %10.2lf  %10.2lf", node_x, node_y) ;
		Write_info(3, buffer) ;
		sleep(1) ;
#endif DEBUG
		if (! *beg_node)
		{
			if ((fabs(node_x - line_beg_x) < thresh) &&
				(fabs(node_y - line_beg_y) < thresh))
			{
#ifdef DEBUG
				Write_info(3, " Beginning node match") ;
				sleep(1) ;
#endif DEBUG
				*beg_node = at_node ;
				beg_match = 1 ;
			}
		}

		if (! *end_node)
		{
			if ((fabs(node_x - line_end_x) < thresh) &&
				(fabs(node_y - line_end_y) < thresh))
			{
#ifdef DEBUG
				Write_info(3, " Ending node match") ;
				sleep(1) ;
#endif DEBUG
				*end_node = at_node ;
				end_match = 1 ;
			}
		}
	}
	return(beg_match + end_match) ;
}
