/*  @(#)do_islands.c	2.2  9/1/87  */
#include <stdio.h>
#include "structures.h"

do_islands(f_digit)
	FILE *f_digit ;
{
	int at_isle ;
	int at_area ;
	int got_area ;
	double cur_dist ;
	double new_dist ;
	double check_inside() ;
	double X, Y ;
	int samp_line ;

#ifdef DEBUG
	int i ;
	char buffer[128] ;

	sprintf(buffer, "n_islands = %d", n_islands) ;
	Write_info(2, buffer) ;
	getchar() ;
#endif DEBUG

	if (! n_islands)
		return(0) ;
	
	for (at_isle=1; at_isle<=n_islands; at_isle++)
	{
		if (islands[at_isle].num_lines <= 0)
			continue ;
		got_area = 0 ;

		samp_line = islands[at_isle].line_list[0] ;
		if (samp_line < 0)
			samp_line = -samp_line ;
		X = endpoints[lines[samp_line].endpoint_beg].x ;
		Y = endpoints[lines[samp_line].endpoint_beg].y ;

#ifdef DEBUG
	sprintf(buffer, "isl %d: %10.2lf %10.2lf", at_isle, X, Y) ;
	Write_info(2, buffer) ;
	getchar() ;
#endif DEBUG

		for(at_area=3; at_area<=n_areas; at_area++)
		{
			if (quick_check(at_area, X, Y) == -1)
			{

#ifdef DEBUG
				sprintf(buffer, "area %d failed quick-check", at_area) ;
				Write_info(3, buffer) ;
				getchar() ;
#endif DEBUG
				continue ;
			}

#ifdef DEBUG
				sprintf(buffer, "area %d PASSES quick-check", at_area) ;
				Write_info(3, buffer) ;
				for(i=0; i<areas[at_area].num_lines; i++)
					show_line(f_digit, areas[at_area].line_list[i], "yellow") ;
				getchar() ;
#endif DEBUG

			new_dist = check_inside(f_digit, X, Y, samp_line,
				areas[at_area].num_lines, areas[at_area].line_list) ;
			if (new_dist == 0.0)
			{

#ifdef DEBUG
				sprintf(buffer, "area %d fails check_inside", at_area) ;
				Write_info(3, buffer) ;
				getchar() ;
				for(i=0; i<areas[at_area].num_lines; i++)
					show_line(f_digit, areas[at_area].line_list[i], "gray") ;
#endif DEBUG
				continue ;
			}

#ifdef DEBUG
				sprintf(buffer, "area %d PASSES check_inside", at_area) ;
				Write_info(3, buffer) ;
				getchar() ;
				for(i=0; i<areas[at_area].num_lines; i++)
					show_line(f_digit, areas[at_area].line_list[i], "gray") ;
#endif DEBUG
			if (! got_area)
			{
				cur_dist = new_dist ;
				got_area = at_area ;
			}
			else if (new_dist < cur_dist)
			{
				cur_dist = new_dist ;
				got_area = at_area ;
			}
		}


	/* These are islands inside the UNIVERSE, and should be stored away for 
 	* use in AREA 2
 	*/
		if (! got_area)
			got_area = 2 ;

#ifdef DEBUG
		sprintf(buffer, "Island inside area %d", got_area) ;
		Write_info(3, buffer) ;
		getchar() ;
#endif DEBUG
		
	/*  these are islands inside the map  */

		if (areas[got_area].n_islands == 0)
			areas[got_area].island_list =
				(int *) falloc(1, sizeof(int)) ;
			    /*************
				(int *) falloc(areas[got_area].n_islands, sizeof(int)) ;
			    *************/
		else
			areas[got_area].island_list =
				(int *) frealloc(
					(char *)areas[got_area].island_list,
					areas[got_area].n_islands + 1,
					sizeof(int)) ;

		areas[got_area].island_list[areas[got_area].n_islands] = at_isle ;
		areas[got_area].n_islands++ ;
	}

	return(1) ;
}
