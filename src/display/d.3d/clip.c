#include <stdio.h>
static int s ;
static int n ;
static int w ;
static int e ;

save_edges(T, B, L, R)
	int T, B, L, R ;
{
	s = B ;
	n = T ;
	w = L ;
	e = R ;
}

draw_line(do_zero,x1,y1,x2,y2)
{
	if(! do_zero)
	{
	    if (x1 == 0 && y1 == 0)
		    return ;
	    if (x2 == 0 && y2 == 0)
		    return ;
	}
	if (  x1 < w || x2 < w
	   || x1 > e || x2 > e
	   || y1 < n || y2 < n
	   || y1 > s || y2 > s)
		return ;
	R_move_abs(x1, y1) ;
	R_cont_abs(x2, y2) ;
}

draw_polygon(x, y, num)
	int *x, *y, num ;
{
	register int *xptr ;
	register int *yptr ;
	int i ;

/* If any vertice of polygon is outside window */
	for(i=0, xptr = x, yptr = y; i<num; i++, xptr++, yptr++)
		if (*xptr < w || *xptr > e || *yptr < n || *yptr > s) 
			return ;
	
	R_polygon_abs(x, y, num) ;
}

draw_polyline(x, y, num)
	int *x, *y, num ;
{
	register int *xptr ;
	register int *yptr ;
	int i ;
	int current ;

	current = 0 ;

	for(i=0, xptr = x, yptr = y; i<num; i++, xptr++, yptr++)
	{
		if (*xptr < w || *xptr > e || *yptr < n || *yptr > s) 
		{
			current = 0 ;
		}
		else
		{
			if(current)
			{
				R_cont_abs(*xptr, *yptr) ;
			}
			else
			{
				R_move_abs(*xptr, *yptr) ;
				current = 1 ;
			}
		}
	}
}
