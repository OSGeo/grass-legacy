/*  @(#)p_to_line.c	2.1  6/26/87  */
#include "dlg.h"
#include <math.h>

/*
*  functions: point_to_line(), line_q_check(), check_dist(), center_check().
*  point_to_line() loops thru all the lines, calling line_q_check() to see
*  if the line is in the line bounding_box.
*    then call check_dist() to get distance of the closest point on the line.
*      if two distances are the same call center_check()  to compare the
*      two distance's away from the center of the bounding box.
*/

point_to_line(ux, uy)
	double ux, uy ;
{
	int choice ;
	char buffer[128] ;
	double new_dist ;
	double cur_dist ;
	int gotone ;
	int a ;

	gotone = 0 ;
	choice = 0 ;

	for(a=1; a<=tot_lines; a++)
	{
		if (line_q_check(a, ux, uy))
		{
			check_dist(a, ux, uy, &new_dist) ;
			if ( (++gotone == 1) || (new_dist <= cur_dist) )
			{
				if (new_dist == cur_dist)
				{
					choice =  center_check( choice, a, ux, uy) ;
				}
				else
				{
					choice = a ;
					cur_dist = new_dist ;
				}
			}
		}
	}

	return (choice) ;

}

line_q_check(a, x, y)
	int a ;
	double x, y ;
{
	if (x <= line[a].W)
		return(0) ;
	if (x >= line[a].E)
		return(0) ;
	if (y <= line[a].S)
		return(0) ;
	if (y >= line[a].N)
		return(0) ;
	
	return(1) ;
}

check_dist(a, ux, uy, dist) 
	int a ;
	double ux ;
	double uy ;
	double *dist ;
{
	double new_dist ;
	double hypot() ;
	double *x ;
	double *y ;
	int n_points ;

	read_coors(a) ;
	n_points = line[a].n_coors ;

	x = coors ;
	y = coors+1 ;

	*dist = hypot(*x - ux, *y - uy) ;
	n_points-- ;

	while (n_points--)
	{
		x += 2 ;
		y += 2 ;
		new_dist = hypot(*x - ux, *y - uy) ;
		if (new_dist < *dist)
			*dist = new_dist ;
	}

	/*  when the universe box is composed of one line the bounding box is
	*	overly large which results in the line being found too frequently.
	*   which is annoying when labeling lines.  need to make a check to 
	*	see if it is really close to the universe line.
	*/
}

center_check( a, b, ux, uy)
	int  a,  b ;
	double  ux,  uy ;
{

	double  dist_a ;
	double  dist_b ;

	dist_a = line[a].N - uy ;
	dist_b = line[b].N - uy ;
	if (dist_a < dist_b)
		return(a) ;
	if (dist_a > dist_b)
		return(b) ;

	dist_a = uy - line[a].S ;
	dist_b = uy - line[b].S ;
	if (dist_a < dist_b)
		return(a) ;
	if (dist_a > dist_b)
		return(b) ;


	dist_a = line[a].E - ux ;
	dist_b = line[b].E - ux ;
	if (dist_a < dist_b)
		return(a) ;
	if (dist_a > dist_b)
		return(b) ;

	dist_a = ux - line[a].W ;
	dist_b = ux - line[b].W ;
	if (dist_a > dist_b)
		return(b) ;

	
	return(a) ;
}

