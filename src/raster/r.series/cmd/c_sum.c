#include "gis.h"

DCELL c_sum(DCELL *values, int n)
{
	double sum;
	int i;

	sum = 0.0;

	for (i = 0; i < n; i++)
		sum += values[i];

	return sum;
}

