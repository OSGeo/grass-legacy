Polydots_rel(xarray, yarray, number)
	int *xarray, *yarray ;
	int number ;
{
	extern int SCREEN_TOP    ;
	register int i ;
	register int *xptr, *yptr ;
	extern int current_x_pos ;
	extern int current_y_pos ;
	xptr = xarray ;
	yptr = yarray ;
	for(i=0; i<number; i++)
	{
		current_x_pos += *xptr ;
		current_y_pos += *yptr ;
		Move_abs(current_x_pos,current_y_pos);
		Cont_abs(current_x_pos+1,current_y_pos);
		xptr++ ;
		yptr++ ;
	}
}
