#include <math.h>
#include "gis.h"
#include "local_proto.h"

DCELL c_stddev(DCELL *values, int n)
{
	return (DCELL) sqrt(d_var(values, n));
}

