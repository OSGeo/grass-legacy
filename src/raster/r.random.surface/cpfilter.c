/* cpfilter.c								*/

#define TRACE
#undef TRACE
#define DEBUG
#undef DEBUG

#undef MAIN
#include "ransurf.h"

CopyFilter( FPtr, Filter)
	FILTER	*FPtr, Filter;
{
	FUNCTION(CopyFilter);

	FPtr->Mult = Filter.Mult;
	FPtr->MaxDist = Filter.MaxDist;
	FPtr->MaxSq = Filter.MaxSq;
	FPtr->Exp = Filter.Exp;
}
