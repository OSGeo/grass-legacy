#include "gis.h"
#include "ncb.h"
CELL
c_intr (values, n)
    register int n;
    register CELL *values;
{
    register int count;
    register int diff;
    CELL center;

    center = *ncb.center;
    if (center == 0) return ((CELL) 0);
    count = 0;
    diff  = 0;
    while (n-- > 0)
    {
	if (*values)
	{
	    count++;
	    if (center != *values)
		diff++;
	}
	values++;
    }
    if (--count <= 0) return ((CELL) 0);
    return ((CELL)((diff*100+(count>>1)) / count) + 1);
}
