#include <grass/gis.h>
#include "ncb.h"
#include "local_proto.h"

DCELL c_mode (
    DCELL *values,
    register int n,
    RASTER_MAP_TYPE map_type)
{
    DCELL center;
    DCELL max;
    int count;
    DCELL mode;
    DCELL prev;

/* sort the array of values, then count each value */
    sort_cell (values, n, 1);
    center = *ncb.center;
    mode = prev = *values++;
    count = 1;
    max = 0;
    while (--n > 0)
    {
	if(G_is_d_null_value(values))
	{
	   values++;
	   continue;
        }
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
