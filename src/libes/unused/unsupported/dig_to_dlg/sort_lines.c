/*  @(#)sort_lines.c	2.1  6/26/87  */
#include <stdio.h>
#include "structures.h"

sort_lines_on_nodes()
{
	int line_sorter() ;
	int at_node ;
#ifdef DEBUG
	char buffer[128] ;
#endif DEBUG

	for(at_node=1; at_node<=n_nodes; at_node++)
	{
		if (node_lines[at_node].n_lines == 0)
			continue ;
		
		qsort(node_lines[at_node].lines,
			node_lines[at_node].n_lines,
			sizeof(int),
			line_sorter) ;

#ifdef DEBUG
		sprintf(buffer,"Node %d #: %d Order: %d %d %d %d",
			at_node,
			node_lines[at_node].n_lines,
			node_lines[at_node].lines[0],
			node_lines[at_node].lines[1],
			node_lines[at_node].lines[2],
			node_lines[at_node].lines[3]) ;
		Write_info(3, buffer) ;
		getchar() ;
#endif DEBUG
	}
}

line_sorter(line1, line2)
	int *line1, *line2 ;
{
	double ang1, ang2 ;

	if (*line1 < 0)
		ang1 = endpoints[lines[- *line1].endpoint_end].angle ;
	else
		ang1 = endpoints[lines[*line1].endpoint_beg].angle ;

	if (*line2 < 0)
		ang2 = endpoints[lines[- *line2].endpoint_end].angle ;
	else
		ang2 = endpoints[lines[*line2].endpoint_beg].angle ;
	
	if (ang1 < ang2)
		return(-1) ;
	if (ang1 > ang2)
		return(1) ;
	return(0) ;
}
