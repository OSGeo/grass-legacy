/*  @(#)calc_angl.c	1.2  7/13/87  */
calc_angles(n_points, xarray, yarray, thresh, beg_ang, end_ang)
	int n_points ;
	double *xarray ;
	double *yarray ;
	double thresh ;
	double *beg_ang ;
	double *end_ang ;
{
	double last_x ;
	double last_y ;
	double *xptr ;
	double *yptr ;
	double fabs() ;
	double atan2() ;
	int is_one_point ;
	int i ;

	last_x = *xarray  ;
	last_y = *yarray  ;
	xptr = xarray + 1 ;
	yptr = yarray + 1 ;

	is_one_point = 1 ;
	for(i=1; i<n_points; i++)  /* Search for next different coord */
	{
		if ( (thresh < fabs(*xptr - last_x) ) ||
			 (thresh < fabs(*yptr - last_y) ) )
		{
			is_one_point = 0 ;
			break ;
		}
		xptr++ ;  yptr++ ;
	}

/****  DEBUG
{
	char	buf[80] ;
	sprintf (buf, "l_x %lf,   l_y %lf,   is_one: %d",
		last_x, last_y, is_one_point) ;
	Write_info(3,buf);
	if (is_one_point)
		sprintf (buf, " x:%lf, y:%lf ", *(xptr-1), *(yptr-1)) ;
	else
		sprintf (buf, " x:%lf, y:%lf ", *xptr, *yptr) ;
	Write_info(4,buf);
	getchar() ;
}
************/

	if (is_one_point)
		return(-1) ;

	*beg_ang = atan2(*yptr-last_y, *xptr-last_x) ;

	last_x = *(xarray + n_points - 1) ;
	last_y = *(yarray + n_points - 1) ;
	xptr = xarray + n_points - 2 ;
	yptr = yarray + n_points - 2 ;
	for(i=n_points-2; i>=0; i--)
	{
		if ( (thresh < fabs(*xptr - last_x) ) ||
			 (thresh < fabs(*yptr - last_y) ) )
			 break ;
		xptr-- ; yptr-- ;
	}

	*end_ang = atan2(*yptr-last_y, *xptr-last_x) ;

	return(0) ;
}
