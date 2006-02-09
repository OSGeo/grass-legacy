#include <math.h>
#include <grass/gis.h>
#include "local_proto.h"

DCELL c_stddev (
    register DCELL *values,
    int n,
    RASTER_MAP_TYPE map_type)
{
    return (CELL_TYPE == map_type? (DCELL)(sqrt(d_var(values,n,map_type)) + .5):
	    (DCELL)sqrt(d_var(values,n,map_type)) );
}
