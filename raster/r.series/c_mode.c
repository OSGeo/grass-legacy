#include "gis.h"
#include "local_proto.h"

void c_mode(DCELL *result, DCELL *values, int n)
{
	DCELL mode;
	int max;
	DCELL prev;
	int count;
	int i;

	n = sort_cell(values, n);

	max = 0;
	count = 0;

	for (i = 0; i < n; i++)
	{
		if (max == 0 || values[i] != prev)
		{
			prev = values[i];
			count = 0;
		}

		count++;

		if (count > max)
		{
			max = count;
			mode = prev;
		}
	}

	if (max == 0)
		G_set_d_null_value(result, 1);
	else
		*result = mode;
}

