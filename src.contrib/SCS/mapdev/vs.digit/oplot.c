/*
**  Last modified by Dave Gerdes  5/1988
**  US Army Construction Engineering Research Lab
*/

#include "digit.h"
plot_points(type, n_coors, xarray, yarray, line_color, point_color)
	char type ;
	int n_coors ;
	double *xarray ;
	double *yarray ;
	int	line_color ;
	int	point_color ;
{
	double *xptr, *yptr ;
	int i ;

	if (type == LINE || type == AREA)
	{
		R_standard_color( dcolors[line_color]) ;
		xptr = xarray ;
		yptr = yarray ;

		First(xptr++, yptr++) ;

		for(i=n_coors-1; i>0; i--)
		{
			Next(xptr++, yptr++) ;
		}

		if(point_color)
		{
			R_standard_color( dcolors[point_color]) ;
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
		R_standard_color( dcolors[point_color]) ;
		xptr = xarray ;
		yptr = yarray ;
		for(i=n_coors; i>0; i--)
			Blot(xptr++, yptr++) ;
	}
	
	R_flush() ;
}
