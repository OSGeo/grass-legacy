Polygon_abs(xarray, yarray, number)
	int *xarray, *yarray ;
	int number ;
{
	int *yarray_int ;
	register int *old_y ;
	register int *new_y ;
	register int n ;
	extern int SCREEN_BOTTOM ;

	check_alloc_array(number, &yarray_int) ;

	old_y = yarray ;
	new_y = yarray_int ;
	n = number ;

	while(n--)
		*new_y++ = SCREEN_BOTTOM - *old_y++ ;

	mgipoly(number, xarray, yarray_int) ;
}
