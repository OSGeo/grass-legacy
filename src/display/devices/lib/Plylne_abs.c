#include "driverlib.h"
int 
Polyline_abs (int *xarray, int *yarray, int number)
{
	int i ;

	Move_abs(xarray[0], yarray[0]) ;

	for(i=1; i<number; i++)
		Cont_abs(xarray[i], yarray[i]) ;

	return 0;
}
