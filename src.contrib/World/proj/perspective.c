#include "map.h"

float viewpt ;

Xperspective (p, x, y) struct place *p ; float *x, *y ;
{
float r ;

if (viewpt <= 1. && p->nlat.s <= viewpt + .01)
	return (-1) ;
r = p->nlat.c * (viewpt - 1.) / (viewpt - p->nlat.s) ;

*x = -r * p->wlon.s ;
*y = -r * p->wlon.c ;

if (r > 3.)
	return (0) ;
if (abs (viewpt) > 1. && p->nlat.s <= 1. / viewpt)
	return (0) ;
return (1) ;
}

perspective (radius) float radius ;
{
extern Xorthographic () ;

viewpt = radius ;
if (radius >= 1000.)
	return (Xorthographic) ;
if (abs (radius - 1.) < .01)
	exit () ;
return (Xperspective) ;
}

stereographic ()
{
viewpt = -1. ;

return (Xperspective) ;
}

Xstereographic (p, x, y)
{
viewpt = -1 ;

return (Xperspective (p, x, y)) ;
}

gnomonic ()
{
viewpt = 0. ;

return (Xperspective) ;
}
