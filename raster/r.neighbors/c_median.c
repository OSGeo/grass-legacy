#include "gis.h"
#include "ncb.h"
#include "local_proto.h"

DCELL c_median (
    DCELL *values,
    register int n,
    RASTER_MAP_TYPE map_type)
{
    DCELL median;

/* sort the array of values, then get median */
    sort_cell (values, n, 1);
    median = (values[(n-1)/2] + values[n/2]) / 2;
    return median;
}
