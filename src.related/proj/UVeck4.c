#ifndef lint
static char *SCCSID = "@(#)UVeck4.c	USGS v.3.1";
#endif
/* Eckert IV projection */
# include	"projects.h"
# define C1	.42223820031577120149
# define C2	1.32650042817700232218
# define RC2	.75386330736002178205
# define C3	3.57079632679489661922
# define RC3	.28004957675577868795
# define EPS	1e-10
# define EPS10	1e-10
# define NITER	10
#ifdef FOR_CODE
	static double
theta(ph) double ph; {
	double th, dth, s, c;
	int i;

	th = .5 * ph;
	ph = C3 * sin(ph);
	for (i = NITER; i ; --i) {
		c = cos(th);
		s = sin(th);
		th += ( dth =
		   (ph - th - (c + 2.) * s) / (2. * c * (1. + c)) );
		if (fabs(dth) < EPS)
			break;
	}
	return th;
}
FORWARD(s_forward); /* spheroid */
	x = C1 * lam * (1. + cos(phi = theta(phi)));
	y = C2 * sin(phi);
	return (xy);
}
#else
NULL_FORWARD(s_forward);
#endif
#ifdef INV_CODE
INVERSE(s_inverse); /* spheroid */
	double c, s;

	if ((s = fabs(y *= RC2)) < 1.) {
		c = cos(s = asin(y));
		phi = asin((s + y * (c + 2.)) * RC3);
		lam = x / (C1 * (1. + c));
	} else if ((s - EPS10) > 1. ) I_ERROR
	else {
		lam = 0.;
		phi = y < 0. ? - HALFPI : HALFPI;
	}
	return (lp);
}
#else
NULL_FORWARD(s_inverse);
#endif
ENTRY(eck4) { if (inverse) RETURN(s_inverse); else RETURN(s_forward); }
