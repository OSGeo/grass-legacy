#include "gis.h"
#include "local_proto.h"

DCELL c_median(DCELL *values, int n)
{
	sort_cell(values, n);
	return (values[(n-1)/2] + values[n/2]) / 2;
}

