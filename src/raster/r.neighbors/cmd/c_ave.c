#include "gis.h"
CELL
c_ave (values, n)
    register CELL *values;
{
    register double sum;
    register int i;

    if(n==0) return 0;
    sum = 0.0;
    for (i = 0; i < n; i++)
	sum += *values++;


    return (CELL)(sum/n + .5);
}
