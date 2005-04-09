#include <math.h>
#include "gis.h"
#include "local_proto.h"

static void percentile(DCELL *result, DCELL *values, int n, double percent)
{
	double k;
	int i0, i1;

	n = sort_cell(values, n);

	if (n < 1)
	{
		G_set_d_null_value(result, 1);
		return;
	}

	k = n * percent / 100;
	i0 = (int) floor(k);
	i1 = (int) ceil(k);

	*result = (i0 == i1)
		? values[i0]
		: values[i0] * (i1 - k) + values[i1] * (k - i0);
}

void c_quart1(DCELL *result, DCELL *values, int n)
{
	return percentile(result, values, n, 25.0);
}

void c_quart3(DCELL *result, DCELL *values, int n)
{
	return percentile(result, values, n, 75.0);
}

void c_perc90(DCELL *result, DCELL *values, int n)
{
	return percentile(result, values, n, 90.0);
}

