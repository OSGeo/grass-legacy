#include <grass/gis.h>
#include "ncb.h"

DCELL c_intr (
    register DCELL *values,
    register int n,
    RASTER_MAP_TYPE map_type)
{
    register int count;
    register int diff;
    DCELL center;
    DCELL no_data;

    G_set_d_null_value(&no_data, 1);
    center = *ncb.center;
    if (G_is_d_null_value(&center)) return no_data;
    count = 0;
    diff  = 0;
    while (n-- > 0)
    {
	if (!G_is_d_null_value(values))
	{
	    count++;
	    if (center != *values)
		diff++;
	}
	values++;
    }
    if (--count <= 0) return ((DCELL) 0);
    return ((DCELL)((diff*100+(count>>1)) / count) + 1);
}
