#include <grass/gis.h>

double d_var (
    register DCELL *values,
    int n,
    RASTER_MAP_TYPE map_type)
{
    register double ave;
    register double sum;
    double d;
    register int i;

    if (n == 0) return 0;
    ave = 0;
    for (i = 0; i < n; i++)
	ave += values[i];

    ave /= (double) n;
    sum = 0;
    for (i = 0; i < n; i++)
    {
	if(!G_is_d_null_value(values))
	{
	   d = values[i] - ave;
	   sum += d*d;
	}
    }

    return sum/n;
}

DCELL
c_var(
    register DCELL *values,
    int n,
    RASTER_MAP_TYPE map_type)
{
    return (CELL_TYPE == map_type? (DCELL)(d_var(values, n, map_type) + .5):
	    (DCELL)d_var(values, n, map_type) );
}

