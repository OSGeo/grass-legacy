#include "gis.h"

CELL
c_min (values, n)
    register int n;
    register CELL *values;
{
    register CELL min;

    min = *values++;
    while (--n > 0)
    {
	if (min > *values)
	    min = *values;
	values++;
    }
    return min;
}
