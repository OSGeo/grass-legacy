#ifndef lint
static char *SCCSID = "@(#)XYeisen.c	USGS v.3.1";
#endif
/*  Eisenlohr Projection */
# include	"projects.h"
# define RSQTWO	0.70710678118654752440
# define FACT	5.82842712474619009760
#ifdef FOR_CODE
FORWARD(s_forward); /* spheroid */
	double s1, c1, t, c, v, cp2, cps, cp;

	s1 = sin(lam *= 0.5);
	c1 = cos(lam);
	cp = cos(phi);
	cp2 = cos(phi *= 0.5);
	t = sin(phi)/(cp2 + 2. * (cps = RSQTWO * sqrt(cp)) * c1);
	c = sqrt(2./(1. + t * t));
	v = sqrt((cp2 + cps * (c1 + s1)) / ( cp2 +
		cps * (c1 - s1)));
	x = FACT * ( -2. * log(v) + c * (v - 1./v));
	y = FACT * ( -2. * atan(t) + c * t * (v + 1./v));
	return (xy);
}
#else
NULL_FORWARD(s_forward);
#endif
NULL_INVERSE(s_inverse);
ENTRY(eisen) { if (inverse) RETURN(s_inverse); else RETURN(s_forward); }
