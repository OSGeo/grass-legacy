/*  @(#)line_rout.c	2.1  6/26/87  */
#define FORWARD		1
#define BACKWARD	-1
#include "structures.h"

check_line(at_line)
	int at_line ;
{
	int abs_at_line ;

	abs_at_line = abs(at_line) ;
	if (at_line == abs_at_line)
	{
		if (lines[abs_at_line].right != UNUSED)
			return 0 ;
		lines[abs_at_line].right = USED ;
		return FORWARD ;
	}
	else
	{
		if (lines[abs_at_line].left != UNUSED)
			return 0 ;
		lines[abs_at_line].left = USED ;
		return BACKWARD ;
	}
}

uncheck_line(at_line)
	int at_line ;
{
	int abs_at_line ;
#ifdef DEBUG
	char buffer[128] ;

	sprintf(buffer, "Unchecking %d\n", at_line) ;
	Write_info(3, buffer) ;
	getchar() ;
#endif DEBUG

	abs_at_line = abs(at_line) ;
	if (at_line == abs_at_line)
		lines[abs_at_line].right = UNUSED ;
	else
		lines[abs_at_line].left = UNUSED ;
}

next_line(node,line,num)
	int node, line ;
	int num ;
{
	int i, j ;
	int num_lines ;
	int *line_list ;

	line = -line ;
	num_lines = node_lines[node].n_lines ;
	line_list = node_lines[node].lines ;

	for (i=0; i<num_lines; i++)
		if (line_list[i] == line)
			break ;
		
	if (num_lines == 1)
		return 0 ;    /* ERROR */
	
	j = i ;
	i = (i+num)%num_lines ;
	while(i != j)
	{
		/* If this line is not an area boundary, continue */
		if (lines[abs(line_list[i])].dig_type != 'L')
			return (line_list[i]) ;
		i = (i+1)%num_lines ;
	}

	return(0) ;
}
