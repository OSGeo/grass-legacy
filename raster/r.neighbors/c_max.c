#include <grass/gis.h>

DCELL
c_max (
    register DCELL *values,
    register int n,
    RASTER_MAP_TYPE map_type)
{
    register DCELL max;

    max = *values++;
    while (--n > 0)
    {
	if(G_is_d_null_value(values)) 
	{
	  values++;
	  continue;
        }
	if (max < *values)
	    max = *values;
	values++;
    }
    return max;
}
