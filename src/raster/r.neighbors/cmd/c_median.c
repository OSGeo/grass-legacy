#include "gis.h"
#include "ncb.h"
#include "local_proto.h"

DCELL c_median (
    DCELL *values,
    register int n,
    RASTER_MAP_TYPE map_type)
{
    DCELL median;
    register int i;

/* sort the array of values, then get median */
    sort_cell (values, n, 1);

    for (i = 0; i < n; i++)
    {
if(G_is_d_null_value(values))
        {
           values++;
           continue;
        }
    median = (values[(n-1)/2] + values[n/2]) / 2;
}
    return median;
}
