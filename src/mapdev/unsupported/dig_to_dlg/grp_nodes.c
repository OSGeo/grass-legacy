/*  @(#)grp_nodes.c	2.1  6/26/87  */
#include <stdio.h>
#include "structures.h"
#define M_NODE_LIST	20

group_nodes()
{
	int num_printed ;
	int at_line ;
	int at_node ;
	int node_list[M_NODE_LIST] ;
	int n_node_refs ;
	int i ;
#ifdef DEBUG
	char buffer[128] ;
#endif DEBUG

/* Allocate initial buffer for holding list of lines associated with nodes */
	node_lines = (struct node_lines *) falloc(ALLOC_AMT,
		sizeof(struct node_lines)) ;

/* Search all lines for nodes */
	for(at_node=1; at_node<=n_nodes; at_node++)
	{
#ifdef DEBUG
		sprintf(buffer," Node %d of %d", at_node, n_nodes) ;
		Write_info(2, buffer) ;
#endif DEBUG
		at_line = 1 ;
		n_node_refs = 0 ;

		while (at_line <= n_lines)
		{
#ifdef DEBUG
		sprintf(buffer," Line %d of %d", at_line, n_lines) ;
		Write_info(3, buffer) ;
#endif DEBUG
			if (endpoints[lines[at_line].endpoint_beg].node == at_node)
			{
				node_list[n_node_refs++] = at_line ;
				if (n_node_refs >= M_NODE_LIST)
					goto yucko ;
			}
			if (endpoints[lines[at_line].endpoint_end].node == at_node)
			{
				node_list[n_node_refs++] = - at_line ;
				if (n_node_refs >= M_NODE_LIST)
					goto yucko ;
			}
			at_line++ ;
		}

/* Stash this list of area boundary lines for later use in finding areas */
	/* Allocate more space for line lists if necessary */
		if (at_node >= alloc_node_lines)
		{
			alloc_node_lines = at_node + ALLOC_AMT ;
			node_lines = (struct node_lines *)frealloc(
				(char *)node_lines,
				alloc_node_lines,
				sizeof(struct node_lines)) ;
		}

	/* Allocate space for this list */
		node_lines[at_node].n_lines = n_node_refs ;
		node_lines[at_node].lines = (int *)falloc(n_node_refs,sizeof(int)) ;
		for(i=0; i<n_node_refs; i++)
			node_lines[at_node].lines[i] = node_list[i] ;
#ifdef DEBUG
		sprintf(buffer, "node: %d  n_lines: %d", at_node, n_node_refs) ;
		Write_info(3, buffer) ;
#endif DEBUG
	}
	return(0) ;

yucko:
	Write_info(4, "Too many lines connecting on one node") ;
	sleep(2) ;
	return(-1) ;
}
