/* This subroutine resamples a dense string of x,y coordinates to
 * produce a set of coordinates that approaches hand digitizing.
 * That is, the density of points is very low on straight lines, and
 * highest on tight curves.
 *
 * xarr and yarr - double precision sets of coordinates.
 * num      - the total number of points in the set.
 * thresh   - the distance that a string must wander from a straight
 *            line before another point is selected.
 */

#include <stdio.h>
#define VERY_LARGE	9999999.

struct line_pnts {
    double *x;
    double *y;
    int     n_points;
    int     alloc_points;
};

int
prune (x, y, n_points, thresh)
	double *x, *y ;
	int n_points ;
	double thresh ;
{
	double *ox, *oy, *nx, *ny ;
	double thresh_sq ;
	double half_thresh ;
	double half_thresh_sq ;
	double cur_x, cur_y ;
	double dx, dy ;
	double last_x, last_y ;
	double a1, b1 ;
	double a2, b2 ;
	double int_x, int_y ;
	double tst_dist ;
	int o_num ;
	int n_num ;
	int at_num ;

	if (n_points <= 2)
		return(n_points) ;

	ox = x;
	oy = y;
	nx = x;
	ny = y;

	o_num = n_points ;
	n_num = 0 ;
	half_thresh = thresh / 2 ;
	half_thresh_sq = half_thresh * half_thresh ;
	thresh_sq = thresh * thresh ;

	/* First point is retained */
	cur_x = *ox++ ;
	cur_y = *oy++ ;

	n_num++ ; nx++ ; ny++ ;
	at_num = 1 ;

	/* Search for first point > half_thresh from current point */
	while (at_num < o_num)
	{
		dx = *ox - cur_x ;
		dy = *oy - cur_y ;
		if (dx * dx + dy * dy > half_thresh_sq)
			break ;
		at_num++ ;  ox++ ;  oy++ ;
	}
	if (at_num == o_num)
	{
		n_num = 2 ;
		*nx = *(ox - 1) ; *ny = *(oy - 1) ;
		return(n_num) ;
	}

	/* calculate line equation coefficients */
	if (dx == 0.0)
		a1 = VERY_LARGE ;
	else
		a1 = dy / dx ;                /* y = ax + b */
	b1 = *oy - a1 * *ox ;
	last_x = *ox++ ;
	last_y = *oy++ ;
	at_num++ ;

	/* Main loop.  Look for first point outside of thresh limit around
	 * line;  save last point;  look for next point to define new line;
	 * repeat 
	 */

	while (at_num < o_num)
	{
		/* Look for first point outside of thresh limit */
		if (a1 == 0.0)
			a2 = VERY_LARGE ;
		else
			a2 = - 1 / a1 ;
		while (at_num < o_num)
		{
				/* Calc. coef. of perp. line through current point */
			b2 = *oy - a2 * *ox ;
				/* Solve two equations for common point */
			int_x = (b2 - b1) / (a1 - a2) ;
			int_y = a2 * int_x + b2 ;
				/* Calc. dist between intersection and cur. point */
			dx = *ox - int_x ;
			dy = *oy - int_y ;
			tst_dist = dx * dx + dy * dy ;
				/* Is this > thresh (within thresh band) ? */
			if (tst_dist > thresh_sq)
				break ;
				/* If not, this point is next candidate for next saved point */
			last_x = *ox++ ;  last_y = *oy++ ;
			at_num++ ;
		}

		*nx++ = last_x ;
		*ny++ = last_y ;
		n_num++ ;

		if (at_num == o_num)
			return(n_num) ;

		cur_x = last_x ;
		cur_y = last_y ;

		/* Search for next point > half_thresh from current point */
		while (at_num < o_num)
		{
			dx = *ox - cur_x ;
			dy = *oy - cur_y ;
			if (dx * dx + dy * dy > half_thresh_sq)
				break ;
			at_num++ ;  ox++ ;  oy++ ;
		}
		if (at_num == o_num)
		{
			n_num++ ;
			*nx = *(ox - 1) ; *ny = *(oy - 1) ;
			return(n_num) ;
		}
		if (dx == 0.0)
			a1 = VERY_LARGE ;
		else
			a1 = dy / dx ;                /* y = ax + b */
		b1 = cur_y - a1 * cur_x ;
		last_x = *ox ;
		last_y = *oy ;
	}
	return(n_num) ;
}
