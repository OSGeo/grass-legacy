/****************************************************************/
/***    Random Number Generator (Uniform Deviates 0.0 -> 1.0) ***/
/***                                                          ***/
/****************************************************************/

#include <stdlib.h>
#include "gmath.h"
#include "config.h"

float 
rand1(int seed)
{
#if defined(HAVE_DRAND48)
	if (seed < 0)
		srand48(-seed);

	return (float) drand48();
#else
	if (seed < 0)
		srand(-seed);

	return 1.0f * rand() / RAND_MAX;
#endif
}
