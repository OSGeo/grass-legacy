#include "gis.h"
#include "local_proto.h"

DCELL c_mode(DCELL *values, int n)
{
	int max;
	DCELL prev;
	int count;
	DCELL mode;
	int i;

	sort_cell(values, n);

	mode = values[0];
	max = 1;

	prev = values[0];
	count = 1;

	for (i = 1; i < n; i++)
	{
		if (values[i] == prev)
		{
			count++;
			continue;
		}

		if (count > max)
		{
			max = count;
			mode = prev;
		}

		prev = values[i];
		count = 1;
	}

	if (count > max)
	{
		max = count;
		mode = prev;
	}

	return mode;
}

