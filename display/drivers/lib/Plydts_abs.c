#include "driverlib.h"
int 
Polydots_abs (int *xarray, int *yarray, int number)
{
	register int i ;
	register int *xptr, *yptr ;
	xptr = xarray ;
	yptr = yarray ;
	for(i=0; i<number; i++)
	{
		Move_abs(*xptr,*yptr) ;
		Cont_rel(0,0) ;
		xptr++ ;
		yptr++ ;
	}

	return 0;
}
