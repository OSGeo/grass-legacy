#include "gis.h"
#include "local_proto.h"

DCELL c_divr (DCELL *values, int n)
{
	int count;
	DCELL prev;
	int i;

	/* sort the array of values, then count differences */

	sort_cell(values, n);

	count = 1;
	prev = values[0];

	for (i = 0; i < n; i++)
		if (values[i] != prev)
		{
			prev = values[i];
			count++;
		}

	return (DCELL) count;
}

