#include "gis.h"
CELL
c_ave (values, n)
    register CELL *values;
{
    register CELL ave;
    register int i;

    ave = 0;
    for (i = 0; i < n; i++)
	ave += *values++;

    if (n)
	ave = (ave + n/2)/n;
    
    return ave;
}
