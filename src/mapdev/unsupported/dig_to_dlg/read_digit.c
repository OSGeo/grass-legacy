/*  @(#)read_digit.c	2.1  6/26/87  */
#include "mode.h"
#include "structures.h"
#include "arrays.h"
#include <stdio.h>

#define  DEGENERATE_LINE	999 ;

read_digit(digit, thresh)
	FILE *digit ;
	double thresh ;
{
	char *falloc() ;
	char *frealloc() ;
	char word[20] ;
	char message[128] ;
	double *xptr ;
	double *yptr ;
	double last_x, last_y ;
	int first_dot ;
	int n_points ;
	int type ;
	int error ;
	int i ;
	long ftell() ;
	short is_one_point ;
	double beg_ang ;
	double end_ang ;

	alloc_points     = 1000 ;
	xarray = (double *) falloc(alloc_points, sizeof(double)) ;
	yarray = (double *) falloc(alloc_points, sizeof(double)) ;

	n_lines = 0;
	n_endpts = 0 ;

/* Allocate initial space for info */
	
	alloc_endpoints = ALLOC_AMT ;
	alloc_lines      = ALLOC_AMT ;

	endpoints = (struct endpoints *) falloc(ALLOC_AMT,
		sizeof(struct endpoints)) ;
	lines = (struct lines *) falloc(ALLOC_AMT,
		sizeof(struct lines)) ;

	while(1)
	{
/*
		if ((n_lines % 10) == 0)
			printf("\n      ") ;
*/

		if (0 >= fread(&type, sizeof(int), 1, digit) )
			return(0) ;
		if (0 >= fread(&n_points, sizeof(int), 1, digit) )
			return(1) ;

		if (n_points >= alloc_points)
		{
			alloc_points = n_points + 100 ;
			xarray = (double *)frealloc((char *)xarray, alloc_points, sizeof(double));
			yarray = (double *)frealloc((char *)yarray, alloc_points, sizeof(double));
		}

		switch (type) 
		{
			case DEAD_AREA:
			case DEAD_LINE:
				if (0 >= fread(xarray, sizeof(double), n_points, digit) )
					return(1) ;
				if (0 >= fread(yarray, sizeof(double), n_points, digit) )
					return(1) ;
				break ;

			case AREA:
			case LINE:

				n_lines++ ;
				if (n_lines >= alloc_lines)
				{
					alloc_lines = alloc_lines + ALLOC_AMT ;
					lines = (struct lines *)frealloc(
						(char *)lines,
						alloc_lines,
						sizeof(struct lines)) ;
				}
			/* Get information for area structure for this line */
				lines[n_lines].offset = ftell(digit) ;
				lines[n_lines].n_points = n_points ;

				if (0 >= fread(xarray, sizeof(double), n_points, digit) )
					return(1) ;
				if (0 >= fread(yarray, sizeof(double), n_points, digit) )
					return(1) ;

				switch(type)
				{
				case AREA:
					sprintf (message, "A%d ", n_lines) ;
					Write_info(2, message) ;
					lines[n_lines].right = UNUSED ;
					lines[n_lines].left = UNUSED ;
					lines[n_lines].dig_type = 'A' ;
					plot_points(type, n_points, xarray, yarray, "gray", "none") ;
					break ;
				case LINE:
					sprintf (message, "L%d ", n_lines) ;
					Write_info(2, message) ;
					lines[n_lines].right  = USED ;
					lines[n_lines].left = USED ;
					lines[n_lines].dig_type = 'L' ;
					plot_points(type, n_points, xarray, yarray, "blue", "none") ;
					break ;
				}

				error = calc_angles(n_points, xarray, yarray, thresh, &beg_ang, &end_ang) ;
				/*  only one point to the line.
				*   keep degenerate LINE, skip AREA line.
				*/
				if (error == -1)
				{
					if (type != LINE)
					{
						n_lines-- ;
						continue ; 
					}
					beg_ang = DEGENERATE_LINE ;
					end_ang = beg_ang ;
				}


				if (n_endpts >= alloc_endpoints - 4)
				{
					alloc_endpoints = n_endpts + ALLOC_AMT ;
					endpoints = (struct endpoints *)frealloc(
						(char *)endpoints,
						alloc_endpoints,
						sizeof(struct endpoints)) ;
				}

				lines[n_lines].endpoint_beg = n_endpts ;
				endpoints[n_endpts].y = *yarray ;
				endpoints[n_endpts].x = *xarray ;
				endpoints[n_endpts].angle = beg_ang ;
#ifdef DEBUG
				sprintf (message, " EP %d:  %10.2lf %10.2lf", n_endpts,
					endpoints[n_endpts].x, endpoints[n_endpts].y) ;
				Write_info(4, message) ;
				getchar() ;
#endif DEBUG
				n_endpts++ ;

				lines[n_lines].endpoint_end = n_endpts ;
				endpoints[n_endpts].y = yarray[n_points-1] ;
				endpoints[n_endpts].x = xarray[n_points-1] ;
				endpoints[n_endpts].angle = end_ang ;
#ifdef DEBUG
				sprintf (message, " EP %d:  %10.2lf %10.2lf", n_endpts,
					endpoints[n_endpts].x, endpoints[n_endpts].y) ;
				Write_info(4, message) ;
				getchar() ;
#endif DEBUG
				n_endpts++ ;
				break;

			default:
				Write_info(2, "ERROR: Don't understand input") ;
				sleep(5) ;
				return(1) ;
				break;
		}
	}
}
