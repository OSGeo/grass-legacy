#include "map.h"

struct coord stdp0, stdp1 ;
int (*stereographic ()) () ;
int (*cylequarea ()) () ;
float k ;
float r0sq ;

Xalbers (place, x, y) struct place *place ; float *x, *y ;
{
double r ;

r = sqrt (r0sq - 2 * place->nlat.s / k) ;
if (stdp1.l < 0 && stdp1.c > stdp0.c || stdp0.l < 0 && stdp0.c > stdp1.c)
	r = -r ;

*x = -r * sin (k * place->wlon.l) ;
*y = -r * cos (k * place->wlon.l) ;

return (1) ;
}

int (*albers (par0,par1)) () float par0,par1 ;
{
deg2rad (par0, &stdp0) ;
deg2rad (par1, &stdp1) ;
if (abs (stdp0.c - stdp1.c) < .01)
	{
	if (par0 >  89.8)
		return (stereographic ()) ;
	if (par0 < -89.8)
		return (0) ;
	return (cylequalarea (par0)) ;
	}

k = (stdp1.s + stdp0.s) / 2 ;
r0sq = (1 + stdp0.s * stdp1.s) / (k * k) ;

return (Xalbers) ;
}
