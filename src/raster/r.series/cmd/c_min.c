#include "gis.h"

DCELL c_min(DCELL *values, int n)
{
	DCELL min;
	int i;

	min = values[0];

	for (i = 1; i < n; i++)
		if (min > values[i])
			min = values[i];

	return min;
}

