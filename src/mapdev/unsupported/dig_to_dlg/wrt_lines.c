/*  @(#)write_lines.c	1.2  6/24/87  */
#include <stdio.h>
#include "structures.h"
#include "arrays.h"

write_lines(f_digit, f_dlg, thresh)
	FILE *f_digit ;
	FILE *f_dlg ;
	double thresh ;
{
	int at_line ;
	int n_atts ;
	int n_points ;
	int num_printed ;
	int i ;
	int num ;
	int right, left ;
	char buff[128] ;
	double thresh2 ;
	double N, S, E, W ;

	n_atts = 0 ;
	thresh2 = thresh * thresh ;

	for(at_line=1; at_line<=n_lines; at_line++)
	{
		fseek(f_digit, lines[at_line].offset, 0) ;
		n_points = lines[at_line].n_points ;
		if (0 >= fread(xarray, sizeof(double), n_points, f_digit) )
			goto done ;
		if (0 >= fread(yarray, sizeof(double), n_points, f_digit) )
			goto done ;
		
		xarray[0] = endpoints[lines[at_line].endpoint_beg].x ;
		yarray[0] = endpoints[lines[at_line].endpoint_beg].y ;
		xarray[n_points-1] = endpoints[lines[at_line].endpoint_end].x ;
		yarray[n_points-1] = endpoints[lines[at_line].endpoint_end].y ;
		
		snap_ends(xarray, yarray, &n_points, thresh2) ;
		bound_box(xarray, yarray, n_points, &N, &S, &E, &W) ;
		find_sides(at_line, &right, &left) ;

	/*
		if the side is zero:  default to inside the map (area 2);
		right = (right) ? right : 2 ;
		left = (left) ? left : 2 ;
	*/

		fwrite("L",         sizeof(char),      1, f_dlg) ;
		num = at_line ;
			fwrite (&num,       sizeof(num),       1, f_dlg) ;
		num = endpoints[lines[at_line].endpoint_beg].node,
			fwrite (&num,       sizeof(num),       1, f_dlg) ;
		num = endpoints[lines[at_line].endpoint_end].node,
			fwrite (&num,       sizeof(num),       1, f_dlg) ;
			fwrite (&left,      sizeof(left),      1, f_dlg) ;
			fwrite (&right,     sizeof(right),     1, f_dlg) ;
		fwrite (&n_points,   sizeof(n_points),   1, f_dlg) ;
		fwrite (&n_atts,     sizeof(n_atts),     1, f_dlg) ;
		fwrite (&N,          sizeof(N),          1, f_dlg) ;
		fwrite (&S,          sizeof(S),          1, f_dlg) ;
		fwrite (&E,          sizeof(E),          1, f_dlg) ;
		fwrite (&W,          sizeof(W),          1, f_dlg) ;

		for (i=0; i<n_points; i++)
		{
			fwrite (xarray+i, sizeof(*xarray), 1, f_dlg) ;
			fwrite (yarray+i, sizeof(*yarray), 1, f_dlg) ;
		}

	}
done:
	free(xarray) ;
	free(yarray) ;
}

#define CHECKLEN	20

snap_ends(xarray, yarray, n_points, thresh2)
	double *xarray, *yarray ;
	int *n_points ;
	double thresh2 ;
{
	int i ;
	int beg ;
	int end ;
	int removed ;
	double curdist ;
	double newdist ;
	int closest ;
	double dx ;
	double dy ;
	double fabs() ;


/* Check points near beginning endpoint */
	dx = xarray[1] - xarray[0] ;
	dy = yarray[1] - yarray[0] ;
	curdist = dx * dx + dy * dy ;
	closest = 1 ;
	
	beg = 2 ;
	end = CHECKLEN ;
	if (end > *n_points - 1)
		end = *n_points - 1 ;

	if (curdist > thresh2) ;
	else
		for (i=beg; i<end; i++)
		{
			dx = xarray[i] - xarray[0] ;
			dy = yarray[i] - yarray[0] ;
			newdist = dx * dx + dy * dy ;
			if (newdist > thresh2)
				break ;
			if (newdist < curdist)
			{
				closest = i ;
				curdist = newdist ;
			}
		}

/* Shift the buffer to remove the overshoot */
	if (closest != 1)
	{
		removed = closest - 1 ;
		*n_points = *n_points - removed ;
		for (i=1; i < *n_points; i++)
		{
			xarray[i] = xarray[i+removed] ;
			yarray[i] = yarray[i+removed] ;
		}
	}

/* Check points near ending endpoint */
	dx = xarray[*n_points - 2] - xarray[*n_points - 1] ;
	dy = yarray[*n_points - 2] - yarray[*n_points - 1] ;
	curdist = dx * dx + dy * dy ;
	closest = *n_points - 2 ;

	end = *n_points - CHECKLEN ;
	beg = *n_points - 3 ;
	if (end < 0)
		end = 0 ;
	
	if (curdist > thresh2) ;
	else
		for (i=beg; i>end; i--)
		{
			dx = xarray[i] - xarray[*n_points - 1] ;
			dy = yarray[i] - yarray[*n_points - 1] ;
			newdist = dx * dx + dy * dy ;
			if (newdist > thresh2)
				break ;
			if (newdist < curdist)
			{
				closest = i ;
				curdist = newdist ;
			}
		}

/* Shift the buffer to remove the overshoot */
	if (closest != *n_points - 2)
	{
		yarray[closest+1] = yarray[*n_points-1] ;
		*n_points = closest + 2 ;
	}
}
