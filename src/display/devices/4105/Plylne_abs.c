Polyline_abs(xarray, yarray, number)
	int *xarray, *yarray ;
	int number ;
{
	int *xptr, *yptr ;

	xptr = xarray ;
	yptr = yarray ;

	Move_abs(*xptr++, *yptr++) ;

	while(--number)
		Cont_abs(*xptr++, *yptr++) ;
}
