/*  @(#)do_areas.c	2.3  11/24/87  */
#include "structures.h"
#include <stdio.h>

do_areas(f_digit, thresh)
	FILE *f_digit ;
	double thresh ;
{
	char message[128] ;
	double cent_x, cent_y ;
	double size ;
	int *line_list ;
	int i ;
	int at_n_line ;
	int at_node ;
	int answ ;
	int first_line ;
	int areas_alloc ;
	int islands_alloc ;
	int num_lines ;
	int avail ;
	double n_bound ;
	double s_bound ;
	double e_bound ;
	double w_bound ;

	n_areas = 2 ;			/*  save 1 & 2 for the universe  */
	n_islands = 0 ;
	line_list = NULL ;

	areas = (struct areas *) falloc(ALLOC_AMT, sizeof(struct areas)) ;
	areas_alloc = ALLOC_AMT ;

	islands = (struct islands *) falloc(10, sizeof(struct islands)) ;
	islands_alloc = 10 ;

	/* Loop through all nodes looking for unused lines */
	for (at_node=1; at_node<=n_nodes; at_node++)
	{
		/* Look at each line attached to node */
		for (at_n_line=0; at_n_line<node_lines[at_node].n_lines; at_n_line++)
		{
			first_line = node_lines[at_node].lines[at_n_line] ;

			/* If this line is not an area boundary, skip it */
			if (lines[abs(first_line)].dig_type == 'L')
				continue ;

			/* If line is used in this direction, continue */
			avail = check_line(first_line) ;
			if (avail == 0)  
				continue ;

			/* Have found a new area */

			answ = find_perimeter_auto(first_line, &num_lines, &line_list) ;

			if (answ == 0)
			{
				sprintf(message,"Warning: area (%d lines) not completed", num_lines) ;
				Write_info(3, message) ;
				sleep(3) ;
			}
			else
			{
				if (do_graphics())
				{
					for(i=0; i<num_lines; i++)
						show_line(f_digit, line_list[i], "yellow") ;
				}

				for(;;)
				{
					Clear_info() ;
					answ = curses_yes_no(4, "Accept this perimeter tracing? yes/no/zoom/quit ", "zq") ;
					if (answ == 'q')
						return(-1) ;
					if (answ != 'z')
						break ;
					if (do_window() != -1)
					{
						redraw_window(f_digit) ;
						for(i=0; i<num_lines; i++)
							show_line(f_digit, line_list[i], "yellow") ;
					}
				}
			}

			if (answ == 'n') answ = 0 ;
			if (answ == 0)
			{
				for(i=0; i<num_lines; i++)
				{
					show_line(f_digit, line_list[i], "gray") ;
					uncheck_line(line_list[i]) ;
				}

				if (do_graphics())
					answ = find_perimeter_interact(
							first_line, &num_lines, &line_list,
							f_digit, thresh) ;
			}

			if (answ == 0)
			{
				for(i=0; i<num_lines; i++)
					show_line(f_digit, line_list[i], "gray") ;
				continue ;
			}

			answ = find_area(f_digit, 0, num_lines, line_list, &size, &cent_x, &cent_y, &s_bound, &n_bound, &w_bound, &e_bound) ;
			if (answ == -1)
			{
				sprintf(message, "  Warning: error on area calc") ;
				Write_info(3, message) ;
				sleep(2) ;
				continue ;
			}

			if (size < 0)     /* Have island perimeter */
			{
				/* Make sure enough space is allocated for one more island */
				if (n_islands + 1 >= islands_alloc)
				{
					islands_alloc = islands_alloc + 10 ;
					islands = (struct islands *)frealloc(
						(char *)islands,
						islands_alloc,
						sizeof(struct islands)) ;
				}
#ifdef DEBUG
				sprintf(message,"  Island %d  Total Area: %12.2lf",
					n_islands+1, -size ) ;
				Write_info(1, message) ;
				sprintf(message, "   N_lines: %d Centroid: %10.2lf,%10.2lf\n",
					num_lines, cent_x, cent_y) ;
				Write_info(2, message) ;
#endif DEBUG
				n_islands++ ;
				/* Stash island information */
				islands[n_islands].cent_x = cent_x ;
				islands[n_islands].cent_y = cent_y ;
				islands[n_islands].num_lines = num_lines ;
				D_save(&islands[n_islands].line_list, line_list, num_lines, sizeof(int)) ;
			}
			else
			{
				/* Make sure enough space is allocated for one more area */
				if (n_areas + 1 >= areas_alloc)
				{
					areas_alloc = areas_alloc + ALLOC_AMT ;
					areas = (struct areas *)frealloc(
						(char *)areas,
						areas_alloc,
						sizeof(struct areas)) ;
				}
#ifdef DEBUG
				sprintf(message, "  Area %d  Total Area: %12.2lf",
					n_areas + 1, size ) ;
				Write_info(1, message) ;
				sprintf(message, "   Centroid: %10.2lf,%10.2lf\n",
					cent_x, cent_y) ;
				Write_info(2, message) ;
#endif DEBUG

			/* Stash area information */
				n_areas++ ;
				areas[n_areas].category = 0 ;
				areas[n_areas].cent_x = cent_x ;
				areas[n_areas].cent_y = cent_y ;
				areas[n_areas].n_bound = n_bound ;
				areas[n_areas].s_bound = s_bound ;
				areas[n_areas].e_bound = e_bound ;
				areas[n_areas].w_bound = w_bound ;
				areas[n_areas].num_lines = num_lines ;
				areas[n_areas].n_islands = 0 ;
				D_save(&areas[n_areas].line_list, line_list, num_lines, sizeof(int)) ;
			}
		/* Turn area edge to GRAY here */
			if (do_graphics())
				for(i=0; i<num_lines; i++)
					show_line(f_digit, line_list[i], "gray") ;
		}

	}
	if (line_list)
		free(line_list) ;

	return(0) ;
}
