#include "gis.h"

DCELL c_sum (
    register DCELL *values,
    int n,
    RASTER_MAP_TYPE map_type)
{
    register double sum;
    register int i;

    if(n==0) return 0;
    sum = 0.0;
    for (i = 0; i < n; i++)
    {
	if(G_is_d_null_value(values))  /* redundant - BB */
	{
	  values++;
	  continue;
        }
	sum += *values++;
    }


    return sum;
}
