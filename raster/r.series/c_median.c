#include "gis.h"
#include "local_proto.h"

void c_median(DCELL *result, DCELL *values, int n)
{
	n = sort_cell(values, n);

	if (n < 1)
		G_set_d_null_value(result, 1);
	else
		*result = (values[(n-1)/2] + values[n/2]) / 2;
}

