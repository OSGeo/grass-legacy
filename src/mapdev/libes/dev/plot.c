/*  @(#)plot.c	2.1  6/26/87  */
#define LINE	0
#define AREA	1

plot_points(type, n_coors, xarray, yarray, line_color, point_color)
	int type ;
	int n_coors ;
	double *xarray ;
	double *yarray ;
	char	*line_color ;
	char	*point_color ;
{
	double *xptr, *yptr ;
	int i ;

	if (type == LINE || type == AREA)
	{
		R_standard_color( D_translate_color(line_color)) ;
		xptr = xarray ;
		yptr = yarray ;

		First(xptr++, yptr++) ;

		for(i=n_coors-1; i>0; i--)
		{
			Next(xptr++, yptr++) ;
		}

		if(  strcmp( point_color, "none") )
		{
			R_standard_color( D_translate_color(point_color)) ;
			xptr = xarray ;
			yptr = yarray ;
			for(i=n_coors-1; i>1; i--)
				Dot(xptr++, yptr++) ;

			Blot(xarray, yarray) ;
			Blot(xarray + n_coors - 1, yarray + n_coors - 1) ;
		}
	}
	
	else
	{
		R_standard_color( D_translate_color(point_color)) ;
		xptr = xarray ;
		yptr = yarray ;
		for(i=n_coors; i>0; i--)
			Blot(xptr++, yptr++) ;
	}
	
	R_flush() ;
}
