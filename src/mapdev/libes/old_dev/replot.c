#define LINE	0
#define AREA	1
#define DOT		2

#define DEAD_LINE	4
#define DEAD_AREA	5
#define DEAD_DOT	6
#include <stdio.h>

replot(digit)
	FILE *digit ;
{
	int type ;
	int n_points ;
	double *xarray ;
	double *yarray ;
	int alloc_points ;

	alloc_points     = 1000 ;
	xarray = (double *) falloc(alloc_points, sizeof(double)) ;
	yarray = (double *) falloc(alloc_points, sizeof(double)) ;

	while(1)
	{
		if (0 >= fread(&type, sizeof(int), 1, digit) )
			goto done ;
		if (0 >= fread(&n_points, sizeof(int), 1, digit) )
			goto done ;
		if (n_points >= alloc_points)
		{
			alloc_points = n_points + 100 ;
			xarray = (double *)frealloc((char *)xarray, alloc_points, sizeof(double));
			yarray = (double *)frealloc((char *)yarray, alloc_points, sizeof(double));
		}
		if (0 >= fread(xarray, sizeof(double), n_points, digit) )
			goto done ;
		if (0 >= fread(yarray, sizeof(double), n_points, digit) )
			goto done ;

		if (type == AREA)
			plot_points(type, n_points, xarray, yarray, "grey", "red") ;
		else
			if (type == LINE)
				plot_points(type, n_points, xarray, yarray, "blue", "red") ;
	}

done:
	free(xarray) ;
	free(yarray) ;
}
