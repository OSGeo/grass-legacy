/* addcov.c                                  */

#define TRACE
#undef TRACE
#define DEBUG
#undef DEBUG

#undef MAIN
#include "ransurf.h"

addcov( IValue, JValue, Dist)
double  Dist;
CELL    IValue, JValue;
{
        int     L, i;

    	FUNCTION(addcov);
        for( L = 0; L < NumLags; L++) {
                i = (int) (Dist / Lags[ L].Lag);
                if( i < Lags[ L].SizeArray) {
                        Lags[ L].Covar[ i] += IValue * JValue - 
					Lags[ L].IMean[ i] * Lags[ L].JMean[ i];
                        Lags[ L].SDI[ i] += IValue * IValue -
				Lags[ L].IMean[ i] * Lags[ L].IMean[ i];
                        Lags[ L].SDJ[ i] += JValue * JValue -
				Lags[ L].JMean[ i] * Lags[ L].JMean[ i];
                        Lags[ L].Vario[ i] += (IValue - JValue) *
						(IValue - JValue);
                } else printf("\nDoCorrel: this should never happen\n");
        }
    	FUNCTION(end addcov);
}
