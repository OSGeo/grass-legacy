/* addmean.c					*/

#define TRACE
#undef TRACE
#define DEBUG
#undef DEBUG

#undef MAIN
#include "ransurf.h"

addmean( IValue, JValue, Dist)
double	Dist;
CELL	IValue, JValue;
{
	int	L, i;

    	FUNCTION(addpoints);
	for( L = 0; L < NumLags; L++) {
		i = (int) (Dist / Lags[ L].Lag);
		if( i < Lags[ L].SizeArray) {
			Lags[ L].IMean[ i] += IValue;
			Lags[ L].JMean[ i] += JValue;
			Lags[ L].Ct[ i] += 1;
		} else printf("\nDoCorrel: this should never happen\n");
	}
    	FUNCTION(end addpoints);
}
