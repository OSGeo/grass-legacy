/* interspersion */

#include "ncb.h"
c_intr (values, n)
    register int n;
    register int *values;
{
    register int count;
    int center;

    center = *ncb.center;
    count = 0;
    while (--n > 0)
    {
	if (center != *values++)
	    count++;
    }
    return count;
}
