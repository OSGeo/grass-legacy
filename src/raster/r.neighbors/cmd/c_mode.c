#include "gis.h"
#include "ncb.h"
CELL
c_mode (values, n)
    register int n;
    CELL *values;
{
    CELL center;
    CELL max;
    int count;
    int sum;
    CELL mode;
    CELL prev;

/* sort the array of values, then count each value */
    sort_cell (values, n, 1);
    center = *ncb.center;
    mode = prev = *values++;
    count = 1;
    max = 0;
    while (--n > 0)
    {
	if (prev == *values)
	    count++;
	else
	{
	    if (count > max)
	    {
		max = count;
		mode = prev;
	    }
	    else if (count == max && prev == center)
	    {
		mode = center;
	    }
	    prev = *values;
	    count = 1;
	}
	values++;
    }
    if (count > max)
    {
	max = count;
	mode = prev;
    }
    else if (count == max && prev == center)
    {
	mode = center;
    }
    return mode;
}
