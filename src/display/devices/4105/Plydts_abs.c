Polydots_abs(xarray, yarray, number)
	int *xarray, *yarray ;
	int number ;
{
	register int i ;
	register int *xptr, *yptr ;
	extern int SCREEN_TOP    ;
	extern int current_x_pos ;
	extern int current_y_pos ;
	xptr = xarray ;
	yptr = yarray ;
	for(i=0; i<number; i++)
	{
        Move_abs(*xptr,*yptr);
		Cont_abs(*xptr+1,*yptr);
		current_x_pos = *xptr ;
		current_y_pos = *yptr ;
		xptr++ ;
		yptr++ ;
	}
}
