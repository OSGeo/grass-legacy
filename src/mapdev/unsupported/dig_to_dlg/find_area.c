/*  @(#)find_area.c	2.1  6/26/87  */
#include "structures.h"
#include "arrays.h"
#include <stdio.h>

find_area(f_digit, beg_link, n_links, links, totalarea, cent_x, cent_y, s, n,
w, e)
	FILE *f_digit ;
	int beg_link ;
	int n_links ;
	int *links ;
	double *totalarea ;
	double *cent_x, *cent_y ;
	double *n, *s, *e, *w ;
{
	int cur_link ;
	int end_link ;
	int ab_link ;
	int n_points ;
	int i ;
	double area ;
	double *xptr1, *yptr1 ;
	double *xptr2, *yptr2 ;
	double cent_weight_x, cent_weight_y ;
	double len, tot_len ;
	int init_set ;

	if (! n_links)
		return(-1) ;

	*totalarea = 0.0 ;
	tot_len = 0.0 ;
	cent_weight_x = 0.0 ;
	cent_weight_y = 0.0 ;
	init_set = 0 ;

	end_link = beg_link + n_links - 1 ;
	for(cur_link=beg_link; cur_link<=end_link; cur_link++)
	{
		ab_link = abs(links[cur_link]) ;
		fseek(f_digit, lines[ab_link].offset, 0) ;
		n_points = lines[ab_link].n_points ;
		if (0 >= fread(xarray, sizeof(double), n_points, f_digit) )
			return(-1) ;
		if (0 >= fread(yarray, sizeof(double), n_points, f_digit) )
			return(-1) ;
		
		xarray[0] = endpoints[lines[ab_link].endpoint_beg].x ;
		yarray[0] = endpoints[lines[ab_link].endpoint_beg].y ;
		xarray[n_points-1] = endpoints[lines[ab_link].endpoint_end].x ;
		yarray[n_points-1] = endpoints[lines[ab_link].endpoint_end].y ;
		
		area = 0.0 ;

		xptr1 = xarray ;
		yptr1 = yarray ;
		xptr2 = xarray + 1 ;
		yptr2 = yarray + 1 ;

		if (! init_set)
		{
			*n = *yptr1 ;
			*s = *yptr1 ;
			*e = *xptr1 ;
			*w = *xptr1 ;
			init_set = 1 ;
		}

		for(i=1; i<n_points; i++)
		{
			area += (*xptr2 - *xptr1) * ((*yptr2 + *yptr1) / 2.0 - south_edge) ;
			len = hypot(*xptr1-*xptr2, *yptr1-*yptr2) ;
			cent_weight_x += len * ((*xptr1 + *xptr2) / 2) ;
			cent_weight_y += len * ((*yptr1 + *yptr2) / 2) ;
			tot_len += len ;
			if (*n < *yptr2) *n = *yptr2 ;
			if (*s > *yptr2) *s = *yptr2 ;
			if (*e < *xptr2) *e = *xptr2 ;
			if (*w > *xptr2) *w = *xptr2 ;
			xptr1++ ; xptr2++ ; yptr1++; yptr2++ ;
		}

		if (links[cur_link] > 0)
			*totalarea += area ;
		else
			*totalarea -= area ;
	}
	*cent_x = cent_weight_x / tot_len ;
	*cent_y = cent_weight_y / tot_len ;
	
	return(0) ;
}
