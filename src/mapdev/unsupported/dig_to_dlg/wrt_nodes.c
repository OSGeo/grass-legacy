/*  @(#)write_nodes.c	1.2  6/24/87  */
#include <stdio.h>
#include "structures.h"

write_nodes(f_dlg)
	FILE *f_dlg ;
{
	int at_node ;
	int n_atts ;
	int endpoint ;
	int line ;

	n_atts = 0 ;

	for(at_node=1; at_node<=n_nodes; at_node++)
	{
	/* get coors for this node by referencing a line that ends at this node */
		if ( (line = node_lines[at_node].lines[0]) > 0)
			endpoint = lines[line].endpoint_beg ;
		else
			endpoint = lines[-line].endpoint_end ;

		fwrite("N",     sizeof(char),   1, f_dlg) ;
		fwrite(&at_node,    sizeof(at_node),    1, f_dlg) ;
		fwrite(&endpoints[endpoint].x, sizeof(endpoints[endpoint].x), 1, f_dlg) ;
		fwrite(&endpoints[endpoint].y, sizeof(endpoints[endpoint].y), 1, f_dlg) ;
		fwrite(&node_lines[at_node].n_lines,
			sizeof(node_lines[at_node].n_lines), 1, f_dlg) ;
		fwrite(&n_atts, sizeof(n_atts), 1, f_dlg) ;
		if (node_lines[at_node].n_lines > 0)
			fwrite (node_lines[at_node].lines,
				sizeof(*node_lines[at_node].lines),
				node_lines[at_node].n_lines, f_dlg) ;
	}
}
