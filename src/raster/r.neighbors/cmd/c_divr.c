#include "gis.h"
CELL
c_divr (values, n)
    register int n;
    CELL *values;
{
    CELL count;
    register CELL prev;

/* sort the array of values, then count differences */

    sort_cell (values, n, 1);
    count = 1;
    prev = *values;
    while (n-- > 0)
    {
	if (prev != *values)
	{
	    prev = *values;
	    count++;
	}
	values++;
    }

    return count;
}
