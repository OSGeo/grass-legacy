#include "bmif.h"
#include "gis.h"
#include "dlg.h"

find_area(xarray, yarray, num_verticies )
	double xarray[], yarray[] ;
	int num_verticies ;
{
	int compare() ;
	int node ;
	int row ;
	register double A, B ;
	double delta_x, delta_y ;
	int first_row, last_row ;
	char *G_calloc() ;
	char *G_realloc() ;

	n_elem_alloc  = 5000 ;
	xy = (struct element *) G_calloc(n_elem_alloc, sizeof(struct element)) ;

	/* adjust Y grid coordinates to Y array coordinates */
	area_yadjust(yarray, num_verticies) ;

	n_elements = 0 ;

	num_verticies-- ;    /* Adjustment for the number of vert pairs */
	for (node=0; node<num_verticies; node++)
	{
#ifdef DEBUG
	fprintf(stderr,"(x,y) %.2f:%.2f %.2f:%.2f  ",
		xarray[node], yarray[node], xarray[node+1], yarray[node+1]) ;
#endif DEBUG
		if ( (xarray[node] == ISLAND_MARKER && yarray[node] == ISLAND_MARKER) ||
			 (xarray[node+1] == ISLAND_MARKER && yarray[node+1] == ISLAND_MARKER) ) 
			continue ;
	/*  generate equation  */
		delta_y = yarray[node+1] - yarray[node] ;
		delta_x = xarray[node+1] - xarray[node] ;
		if (delta_y == 0.0) 
			B = 9999999. ;
		else
			B = delta_x / delta_y ;
		A = xarray[node] - B * yarray[node]  ;
#ifdef DEBUG
	fprintf(stderr,"A = %f  B = %f\n", A, B) ;
#endif DEBUG

	/*  determine first and last row involved */
		if (yarray[node+1] > yarray[node])
		{
			if (yarray[node] > 0.0)
				first_row = yarray[node] + 1. ;
			else
				first_row = yarray[node] ;
			if (yarray[node+1] > 0.0)
				last_row = yarray[node+1] ;
			else
				last_row = yarray[node+1] - 1. ;
		}
		else if (yarray[node+1] < yarray[node])
		{
			if (yarray[node+1] > 0.0)
				first_row = yarray[node+1] + 1. ;
			else
				first_row = yarray[node+1] ;
			if (yarray[node] > 0.0)
				last_row = yarray[node] ;
			else
				last_row = yarray[node] - 1. ;
		}

#ifdef DEBUG
fprintf(stderr,"first: %6d  last: %6d\n", first_row, last_row) ;
#endif DEBUG

		if (first_row > last_row)
			continue ;

		if (delta_y == 0.0)
			continue ;

		for (row=first_row; row<=last_row; row++)
		{
			if (n_elements + 1 >= n_elem_alloc)
			{
				n_elem_alloc = n_elem_alloc + 1000 ;
				xy = (struct element *)
					G_realloc((char *)xy, n_elem_alloc * sizeof(struct element));
			}
			xy[n_elements].row = row ;
			xy[n_elements].col = A + B * row ;
#ifdef DEBUG
fprintf(stderr,"%2d %2d %6.2f\n",
	n_elements, xy[n_elements].row, xy[n_elements].col) ;
#endif DEBUG
			n_elements++ ;
		}
	}

	qsort(xy, n_elements, sizeof(struct element), compare) ;

#ifdef DEBUG
	fprintf(stderr,"\n") ;
	for(row=0; row<n_elements; row++)
		fprintf(stderr,"%2d %2d %6.2f\n", row, xy[row].row, xy[row].col) ;
#endif DEBUG
}

static
compare(element1, element2)
	struct element *element1, *element2 ;
{
	if (element1->row < element2->row)
		return(-1) ;
	if (element1->row > element2->row)
		return(1) ;
	if (element1->col < element2->col)
		return(-1) ;
	if (element1->col > element2->col)
		return(1) ;

	return(0) ;
}
