/*  @(#)read_coors.c	2.1  6/26/87  */
#include <stdio.h>
#include "dlg.h"

read_coors(choice)
	int choice ;
{
	FILE *file ;
	int i ;
	int n_lines ;

	file = line[choice].file ;

	if (file == NULL)
		return -2 ;

	alloc_coors(n_lines = line[choice].n_coors) ;

	if (n_lines > 2)
	{
		if (fseek(file,line[choice].offset,0) < 0)
			return -2 ;

		fread (coors, sizeof(*coors), line[choice].n_coors * 2, file) ;
	}

/* Adjust beginning and ending node coordinates */
	coors[0] = node[line[choice].start_node].x ;
	coors[1] = node[line[choice].start_node].y ;
	coors[(n_lines-1) * 2    ] = node[line[choice].end_node].x ;
	coors[(n_lines-1) * 2 + 1] = node[line[choice].end_node].y ;

	return(0) ;
}
