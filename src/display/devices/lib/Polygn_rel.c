#include "driver.h"
#include "driverlib.h"
int 
Polygon_rel (int *xarray, int *yarray, int number)
{
	int incr ;

	xarray[0] += cur_x ;
	yarray[0] += cur_y ;

	for (incr=1; incr<number; incr++)
	{
		xarray[incr] += xarray[incr-1] ;
		yarray[incr] += yarray[incr-1] ;
	}

	incr-- ;

	Polygon_abs(xarray, yarray, number) ;

	return 0;
}
