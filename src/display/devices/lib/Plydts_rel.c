Polydots_rel(xarray, yarray, number)
	int *xarray, *yarray ;
	int number ;
{
	register int i ;
	register int *xptr, *yptr ;
	xptr = xarray ;
	yptr = yarray ;
	for(i=0; i<number; i++)
	{
		Move_rel(*xptr,*yptr) ;
		Cont_rel(*xptr,*yptr) ;
		xptr++ ;
		yptr++ ;
	}
}
