/* decay.c								*/

#define TRACE
#undef TRACE
#define DEBUG
#undef DEBUG

#undef MAIN
#include "ransurf.h"

DistDecay( Effect, R, C)
	double	*Effect;
	int	R, C;
{
	FUNCTION(DistDecay);
	INT(R); INT(C);
	*Effect = BigF.F[ R + BigF.RowPlus][ C + BigF.ColPlus];
	FUNCTION(end DistDecay);
}
