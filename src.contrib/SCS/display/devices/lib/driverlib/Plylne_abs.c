/* %W% %G% */
Polyline_abs(xarray, yarray, number)
	int *xarray, *yarray ;
	int number ;
{
	int i ;

	Move_abs(xarray[0], yarray[0]) ;

	for(i=1; i<number; i++)
		Cont_abs(xarray[i], yarray[i]) ;
}
