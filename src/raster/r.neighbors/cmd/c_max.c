#include "gis.h"

CELL
c_max (values, n)
    register int n;
    register CELL *values;
{
    register CELL max;

    max = *values++;
    while (--n > 0)
    {
	if (max < *values)
	    max = *values;
	values++;
    }
    return max;
}
