#include "gis.h"

void c_var(DCELL *result, DCELL *values, int n)
{
	DCELL sum, ave, sumsq;
	int count;
	int i;

	sum = 0.0;
	count = 0;

	for (i = 0; i < n; i++)
	{
		if (G_is_d_null_value(&values[i]))
			continue;

		sum += values[i];
		count++;
	}

	if (count == 0)
	{
		G_set_d_null_value(result, 1);
		return;
	}

	ave = sum / count;

	sumsq = 0;

	for (i = 0; i < n; i++)
	{
		DCELL d;

		if (G_is_d_null_value(&values[i]))
			continue;

		d = values[i] - ave;
		sumsq += d * d;
	}

	*result = sumsq / count;
}

