/*  @(#)find_perim.c	2.1  6/26/87  */
#include "structures.h"
#include <stdio.h>

static int lines_alloc = 0 ;
static int *line_list ;

find_perimeter_auto(first_line, num_lines, linelist)
	int first_line ;
	int *num_lines ;
	int **linelist ;
{
	int at_line ;
	int answ ;
	int next_node ;
	int avail ;

	if (! lines_alloc)
	{
		line_list = (int *) falloc(ALLOC_AMT, sizeof(int)) ;
		lines_alloc = ALLOC_AMT ;
	}

	/* Trace area */
	*num_lines = 0 ;
	at_line = first_line ;
	answ = 1 ;
	do
	{
		/* Make sure enough space is allocated for one more line */
		if (*num_lines + 1 >= lines_alloc)
		{
			lines_alloc = lines_alloc + ALLOC_AMT ;
			line_list = (int *)frealloc(
				(char *)line_list,
				lines_alloc,
				sizeof(int)) ;
		}

		line_list[(*num_lines)++] = at_line ;
		if(at_line < 0)
			next_node = endpoints[lines[-at_line].endpoint_beg].node ;
		else
			next_node = endpoints[lines[at_line].endpoint_end].node ;

		at_line = next_line(next_node,at_line,1) ;
		if (at_line == 0)   /* i.e. this node has only one line */
		{
			answ = 0 ;
			break ;
		}
		if (at_line == first_line) 
			break ;
		avail = check_line(at_line) ;
		if (avail == 0)
		{
			answ = 0 ;
			break ;
		}
	} while(at_line != first_line) ;

	*linelist = line_list ;
	return(answ) ;
}

find_perimeter_interact(first_line, num_lines, linelist, f_digit, thresh)
	int first_line ;
	int *num_lines ;
	int **linelist ;
	FILE *f_digit ;
	double thresh ;
{
	int at_line ;
	int new_line ;
	int answ ;
	int next_node ;
	int accept_it ;

	if (! lines_alloc)
	{
		line_list = (int *) falloc(ALLOC_AMT, sizeof(int)) ;
		lines_alloc = ALLOC_AMT ;
	}

	/* Trace area */
	*num_lines = 0 ;
	at_line = first_line ;
	answ = 0 ;

	show_line(f_digit, first_line, "yellow") ;

	do
	{
		Clear_info() ;
		/* Make sure enough space is allocated for one more line */
		if (*num_lines + 1 >= lines_alloc)
		{
			lines_alloc = lines_alloc + ALLOC_AMT ;
			line_list = (int *)frealloc(
				(char *)line_list,
				lines_alloc,
				sizeof(int)) ;
		}

		line_list[(*num_lines)++] = at_line ;
		if(at_line < 0)
			next_node = endpoints[lines[-at_line].endpoint_beg].node ;
		else
			next_node = endpoints[lines[at_line].endpoint_end].node ;

	/* Loop to find next line for area */
		for(;;)
		{
			answ = get_next_line_auto(at_line, next_node, &new_line, f_digit, line_list, *num_lines) ;

			if (answ == 0)
			{
				accept_it = curses_yes_no(4, "abandon this area (y/n)? ", "") ;
				if (accept_it == 'y')
				{
					answ = 0 ;
					goto goodbye ;
				}
				continue ;
			}

			break ;
		}
		at_line = new_line ;

	} while(at_line != first_line) ;

goodbye:
	*linelist = line_list ;
	return(answ) ;
}


get_next_line_auto(at_line, next_node, new_line, f_digit, line_list, num_lines)
	int at_line ;
	int next_node ;
	int *new_line ;
	FILE *f_digit ;
	int *line_list ;
	int num_lines ;
{
	int new_l ;
	int num ;
	int accept_it ;
	int a_line ;
#ifdef DEBUG
	char buffer[128] ;
#endif DEBUG

	num = 1 ;

	for(;;)
	{
		accept_it = 0 ;
		*new_line = next_line(next_node,at_line,num++) ;
#ifdef DEBUG
	sprintf(buffer, " Cur. line: %d  node: %d Next line: %d",
		at_line, next_node, *new_line) ;
	Write_info(1, buffer) ;
	getchar() ;
#endif DEBUG
		if (*new_line == 0)    /* i.e. one line on node */
			return(0) ;
		if (*new_line == at_line)
			return(0) ;    /*  need to go to interact */
	
		accept_it = check_line(*new_line) ;
		if (accept_it != 0)
		{
			show_line(f_digit, *new_line, "red") ;
			for(;;)
			{
				accept_it = curses_yes_no(4, "Accept this line? yes/no/zoom ", "z") ;
				if (accept_it != 'z')
					break ;
				if (do_window() != -1)
				{
					redraw_window(f_digit) ;
					for(a_line=0; a_line<num_lines; a_line++)
						show_line(f_digit, line_list[a_line], "yellow") ;
					show_line(f_digit, *new_line, "red") ;
				}
			}
			if (accept_it == 'n')
			{
				show_line(f_digit, *new_line, "gray") ;
				uncheck_line(*new_line) ;
			}
			else
			{
				show_line(f_digit, *new_line, "yellow") ;
				return(1) ;    /* Got next line!! */
			}
		}
	}
}
