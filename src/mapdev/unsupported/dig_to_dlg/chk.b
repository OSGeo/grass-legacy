/*  @(#)chk_inside.c	2.1  6/26/87  */
/* check_inside()  determines if a point (X,Y) is inside an area
 *   described by the "num_lines" in the list "line_list".  If it is
 *   a positive floating point number is returned giving the distance
 *   to the area.  If not, "0.0" is returned.
 */
#include "arrays.h"
#include "structures.h"
#include <stdio.h>

double
check_inside(f_digit, X, Y, samp_line, num_lines, line_list)
	FILE *f_digit ;
	double X, Y ;
	int samp_line ;
	int num_lines ;
	int *line_list ;
{
	double *x, *y ;
	double cur_min ;
	double cur_x, cur_y ;
	double x_intersect() ;
	double x_inter ;
	double fabs() ;
	double x_dist ;
	int at_line ;
	int n_intersects ;
	int n_points ;
	int n_segs ;
	int l ;
	int n ;

	cur_min = 999999999. ;
	cur_x = 0.0 ;
	cur_y = 0.0 ;
	n_intersects = 0 ;

/* Get-line loop */
	for(l=0; l<num_lines; l++)
	{
	/* Read in line coordinates */
		at_line = abs(line_list[l]) ;
		if (at_line == samp_line)
			return(0.0) ;
		fseek(f_digit, lines[at_line].offset, 0) ;
		n_points = lines[at_line].n_points ;
		if (0 >= fread(xarray, sizeof(double), n_points, f_digit) )
			return(-1) ;
		if (0 >= fread(yarray, sizeof(double), n_points, f_digit) )
			return(-1) ;
		xarray[0] = endpoints[lines[at_line].endpoint_beg].x ;
		yarray[0] = endpoints[lines[at_line].endpoint_beg].y ;
		xarray[n_points-1] = endpoints[lines[at_line].endpoint_end].x ;
		yarray[n_points-1] = endpoints[lines[at_line].endpoint_end].y ;
	
	/* adjust yarray coordinates */
		y = yarray ;
		for(n=0; n<n_points; n++)
		{
			if (*y == Y)
				*y = Y + .001 ;
			y++ ;
		}

	/* Point loop */
		x = xarray ;
		y = yarray ;
		cur_x = *x ; x++ ;
		cur_y = *y ; y++ ;
		n_segs = n_points - 1 ;
	
		for(n=0; n<n_segs; n++)
		{
			if((cur_y < Y && *y < Y)
			|| (cur_y > Y && *y > Y))
			{
				cur_x = *x ; x++ ;
				cur_y = *y ; y++ ;
				continue ;
			}

			x_inter = x_intersect (cur_x, *x, cur_y, *y, Y) ;
			if (x_inter > X)
			{
				n_intersects++ ;

				x_dist = fabs(x_inter - X) ;
				if (x_dist < cur_min)
					cur_min = x_dist ;
			}

			cur_x = *x ; x++ ;
			cur_y = *y ; y++ ;
		}
	}
	if (n_intersects % 2)
		return(cur_min) ;
	else
		return(0.0) ;
}

double
x_intersect (beg_x, end_x, beg_y, end_y, Y) 
	double beg_x ;
	double end_x ;
	double beg_y ;
	double end_y ;
	double Y ;
{
	double b, a ;

	b = (end_x - beg_x) / (end_y - beg_y) ;
	a = beg_x - b * beg_y ;
	return(a + b * Y) ;
}

quick_check(area, x, y)
	int area ;
	double x, y ;
{
	if (x <= areas[area].w_bound)
		return(-1) ;
	if (x >= areas[area].e_bound)
		return(-1) ;
	if (y <= areas[area].s_bound)
		return(-1) ;
	if (y >= areas[area].n_bound)
		return(-1) ;
	
	return(0) ;
}
