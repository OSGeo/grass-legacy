#include <stdio.h>
#include "raster.h"

static int s ;
static int n ;
static int w ;
static int e ;

int 
save_edges (int T, int B, int L, int R)
{
	s = B ;
	n = T ;
	w = L ;
	e = R ;

	return 0;
}

int 
draw_line (int do_null, int x1, int y1, int x2, int y2)
{
	if(! do_null)
	{
	    if (x1 == 0 && y1 == 0)
		    return 0;
	    if (x2 == 0 && y2 == 0)
		    return 0;
	}
	if (  x1 < w || x2 < w
	   || x1 > e || x2 > e
	   || y1 < n || y2 < n
	   || y1 > s || y2 > s)
		return 0;
	R_move_abs(x1, y1) ;
	R_cont_abs(x2, y2) ;

	return 0;
}

int 
draw_polygon (int *x, int *y, int num)
{
	register int *xptr ;
	register int *yptr ;
	int i ;

/* If any vertice of polygon is outside window */
	for(i=0, xptr = x, yptr = y; i<num; i++, xptr++, yptr++)
		if (*xptr < w || *xptr > e || *yptr < n || *yptr > s) 
			return 0;
	
	R_polygon_abs(x, y, num) ;

	return 0;
}

int 
draw_polyline (int *x, int *y, int num)
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

	return 0;
}
