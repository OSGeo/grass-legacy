#include "gis.h"

DCELL c_max(DCELL *values, int n)
{
	DCELL max;
	int i;

	max = values[0];

	for (i = 1; i < n; i++)
		if (max < values[i])
			max = values[i];

	return max;
}

