/*  @(#)inside.c	2.1  6/26/87  */
/* check_inside()  determines if a point (X,Y) is inside an area
 *   described by the "n_lines" in the list "line_list".  If it is
 *   a positive floating point number is returned giving the distance
 *   to the area.  If not, "0" is returned.
 */
#include "dlg.h"
#include <stdio.h>


check_inside(a, X, Y, cur_min)
	int a ;
	double X, Y ;
	double *cur_min ;
{
	double *x, *y ;
	double cur_x, cur_y ;
	double x_intersect() ;
	double x_inter ;
	double fabs() ;
	double x_dist ;
	int at_line ;
	int incr ;
	int n_intersects ;
	int n_points ;
	int l ;
	int n ;

	char	buf[100] ;


	*cur_min = 999999999. ;
	cur_x = 0.0 ;
	cur_y = 0.0 ;
	n_intersects = 0 ;

/* Get-line loop */
	for(l=0; l<area[a].n_lines; l++)
	{
		at_line = abs(area[a].lines[l]) ;


		if(at_line == 0)    /* Marks the beginning of an island */
			break ;

		if ( (Y > line[at_line].N) || (Y < line[at_line].S) )
			continue ;
		if (read_coors(at_line) < 0)
		{
			fprintf( stderr, " read_coors cannot read a line\n") ;
			exit(-1) ;
		}
		n_points = line[at_line].n_coors ;

		incr = 2 ;
		y = coors+1 ;

		n = n_points ;
		while(n--)
		{
			y_adjust(y, Y) ;
			y += incr ;
		}

		x = coors ;
		y = coors+1 ;

		cur_x = *x ; x += incr ;
		cur_y = *y ; y += incr ;
		n_points-- ;
	
	/* Point loop */
		for(; n_points--; cur_x = *x, cur_y = *y, x += incr, y +=incr)
		{
			if (cur_y < Y && *y < Y)
				continue ;
			if (cur_y > Y && *y > Y)
				continue ;

			x_inter = x_intersect (cur_x, *x, cur_y, *y, Y) ;

			if (x_inter < X)
				continue ;

			n_intersects++ ;

			x_dist = fabs(x_inter - X) ;
			if (x_dist < *cur_min)
				*cur_min = x_dist ;
		}
	}

	if (n_intersects % 2)
		return(1) ;
	else
		return(0) ;
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

y_adjust(y, Y)
	double *y ;
	double Y ;
{
	if (*y == Y)
		*y = Y + .001 ;
}

quick_check(a, x, y)
	int a ;
	double x, y ;
{
	if (x <= area[a].W)
		return(0) ;
	if (x >= area[a].E)
		return(0) ;
	if (y <= area[a].S)
		return(0) ;
	if (y >= area[a].N)
		return(0) ;
	
	return(1) ;
}
