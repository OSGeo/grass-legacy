
Polyline_rel(xarray, yarray, number)
	int *xarray, *yarray ;
	int number ;
{
	int *xptr, *yptr ;
	int n ;
	extern int current_x_pos ;
	extern int current_y_pos ;

	xptr = xarray ;
	yptr = yarray ;

	*xptr = current_x_pos + *xptr ;
	*yptr = current_y_pos + *yptr ;

	xptr++ ;
	yptr++ ;

	for(n=0; n<number; n++)
	{
		*xptr = *(xptr-1) + *xptr ;
		*yptr = *(yptr-1) + *yptr ;
		xptr++ ;
		yptr++ ;
	}

	Polyline_abs(xarray, yarray, number) ;
}
