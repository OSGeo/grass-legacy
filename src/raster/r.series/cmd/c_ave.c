#include "gis.h"

DCELL c_ave(DCELL *values, int n)
{
	DCELL sum;
	int i;

	sum = 0.0;
	for (i = 0; i < n; i++)
		sum += values[i];

	return sum / n;
}

