/* dd.c								*/

#define TRACE
#undef TRACE
#define DEBUG
#undef DEBUG

#undef MAIN
#include "ransurf.h"

double
DD( Dist)
	double	Dist;
{
	double	SmallD, SmallDist, Power;

	if( Dist < Filter.Mult) 
		return( (double) 1.0);
	SmallD = Filter.MaxDist - Filter.Mult;
	SmallDist = Dist - Filter.Mult;
	return( 1.0 - pow( (SmallDist / SmallD), Filter.Exp));
}
