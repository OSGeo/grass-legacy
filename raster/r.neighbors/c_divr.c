#include <grass/gis.h>
#include "local_proto.h"

DCELL c_divr (
    DCELL *values,
    register int n,
    RASTER_MAP_TYPE map_type)
{
    DCELL count;
    register DCELL prev;

/* sort the array of values, then count differences */

    sort_cell (values, n, 1);
    count = 1;
    prev = *values;
    while (n-- > 0)
    {
	if(G_is_d_null_value(values)) 
	{
	    continue;
	    values++;
        }
	if (prev != *values)
	{
	    prev = *values;
	    count++;
	}
	values++;
    }

    return (DCELL )count;
}
