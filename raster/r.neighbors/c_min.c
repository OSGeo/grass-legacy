#include <grass/gis.h>

DCELL
c_min (
    register DCELL *values,
    register int n,
    RASTER_MAP_TYPE map_type)
{
    register DCELL min;

    min = *values++;
    while (--n > 0)
    {
	if(!G_is_d_null_value(values))
	{
	   if (min > *values)
	       min = *values;
	   values++;
	}
    }
    return min;
}
