Polydots_abs(xarray, yarray, number)
	int *xarray, *yarray ;
	int number ;
{
	register int i ;
	register int *xptr, *yptr ;
	xptr = xarray ;
	yptr = yarray ;
	for(i=0; i<number; i++)
	{
		Move_abs(*xptr,*yptr) ;
		Cont_rel(0,0) ;
		xptr++ ;
		yptr++ ;
	}
}
