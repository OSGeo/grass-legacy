/* calcmean.c					*/

#define TRACE
#undef TRACE
#define DEBUG
#undef DEBUG

#undef MAIN
#include "ransurf.h"

calcmean( )
{
	int	L, i;

    	FUNCTION(calcmeans);
	for( L = 0; L < NumLags; L++) {
	    for( i = 0; i < Lags[ L].SizeArray; i++) {
	      if( Lags[ L].Ct[ i]) {
		Lags[ L].IMean[ i] = Lags[ L].IMean[ i] / Lags[ L].Ct[ i];
		Lags[ L].JMean[ i] = Lags[ L].JMean[ i] / Lags[ L].Ct[ i];
	      }
	    } 
	}
    	FUNCTION(end calcmeans);
}
