#include "map.h"

Xmercator (place,x,y) struct place *place ; float *x, *y ;
{
if (abs (place->nlat.l) > 80. * RAD)
	return (-1) ;

*x = -place->wlon.l ;
*y = 0.5 * log ((1 + place->nlat.s) / (1 - place->nlat.s)) ;

return (1) ;
}

int (*mercator ()) () { return (Xmercator) ; }
