#include "driver.h"
#include "driverlib.h"
int 
Polyline_rel (int *xarray, int *yarray, int number)
{
	int i ;

	Move_rel(xarray[0], yarray[0]) ;

	for (i=1; i<number; i++)
		Cont_rel(xarray[i], yarray[i]) ;

	return 0;
}
