#include <math.h>
#include "gis.h"
#include "local_proto.h"

void c_stddev(DCELL *result, DCELL *values, int n)
{
	DCELL var;

	c_var(&var, values, n);

	if (G_is_d_null_value(&var))
		G_set_d_null_value(result, 1);
	else
		*result = sqrt(var);
}

