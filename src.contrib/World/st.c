#include "map.h"

Xstereographic(place, x, y)
struct place *place;
float *x, *y;
{
	double r;
	if(place->nlat.l < - 80.*RAD)
		return(-1);
	r = (1 + place->nlat.c - place->nlat.s)
		/(1 + place->nlat.c + place->nlat.s);
	/* r = tan(PI/4 - nlat/2) */
	*x = - r * place->wlon.s;
	*y = - r * place->wlon.c;
	return(1);
}

int (*stereographic())()
{
	return(Xstereographic);
}
>nlat.l;
		*y = place->nlat.l * (1+(lon2/2)*(1-(8+lon2)*lat2/12));
		*x = - place->wlon.l * (1-lat2*(3+lon2)/6);
	}
	return(1);
}

int (*po