/*  @(#)bound_box.c	2.1  6/26/87  */
#include	<stdio.h>
#include	"dlg.h"

#define		SCALE	16000.0		
#define		BOX_MIN	20

bound_box(xptr, yptr, n_coors, N, S, E, W)
	double *xptr, *yptr ;
	int n_coors ;
	double *N ;
	double *S ;
	double *E ;
	double *W ;
{

	double		dist ;
	double		half_dist ;

	double	scale ;
	double	atof() ;
	double	fabs() ;

	static  int  box_min = -1 ;	

	/*
	*	on straight lines the bounding box was so small it made it hard to find
	*	the line with a mouse.   first time this is called set the bounding box
	*	minimum limit.  the minimum was calculated at a scale of 16000;
	*	take any scale into account.
	*	
	*	find current distance between (N,S), (E,W).  if its less then box_min
	*	enlarge the box to the minimum;  it also keeps the line in the middle
	*	of the box.
	*/

	if ( box_min <= 0)
	 {
		scale = atof(dlg_head.orig_scale) ;
		if ( scale <= 0)
			box_min = SCALE  /  SCALE  *  BOX_MIN ;
		else
			box_min = scale  /  SCALE  *  BOX_MIN ;
	 }

	if (n_coors == 0)
	{
		*E = 0 ;
		*W = 0 ;
		*N = 0 ;
		*S = 0 ;
		return ;
	}

	*E = *xptr ;
	*W = *xptr ;
	*N = *yptr ;
	*S = *yptr ;

	while(--n_coors)
	{
		xptr++ ;
		yptr++ ;
		if (*xptr < *W) *W = *xptr ;
		if (*xptr > *E) *E = *xptr ;
		if (*yptr < *S) *S = *yptr ;
		if (*yptr > *N) *N = *yptr ;
	}

	/*  have got the bounding box,  now make sure its large enough  */

	dist = (*N - *S) - box_min ;
	if ( dist < 0)
	 {
		half_dist =  fabs (dist)  /  2 ;
		*N += half_dist ;
		*S -= half_dist ;
	 }

	dist = (*E - *W) - box_min ;
	if ( dist < 0)
	 {
		half_dist =  fabs (dist)  /  2 ;
		*E += half_dist ;
		*W -= half_dist ;
	 }

}

