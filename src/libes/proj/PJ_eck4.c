/* Eckert IV projection */
#ifndef lint
static char RCSID[] = "@(#)$Id: PJ_eck4.c,v 4.2 1992/07/14 01:27:20 gie Exp $";
#endif
#define __PJ_LIB
#include	"projects.h"
#define C1	.42223820031577120149
#define C2	1.32650042817700232218
#define RC2	.75386330736002178205
#define C3	3.57079632679489661922
#define RC3	.28004957675577868795
#define EPS	1e-10
#define EPS10	1e-10
#define NITER	10

static double
#ifdef __STDC__
theta(double ph)
#else
theta(ph)
    double ph;
#endif
{
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
FORWARD(s_forward) { XY xy;  /* spheroid */
	xy.x = C1 * lp.lam * (1. + cos(lp.phi = theta(lp.phi)));
	xy.y = C2 * sin(lp.phi);
	return (xy);
}
INVERSE(s_inverse) { LP lp;  /* spheroid */
	double c, s;

	if ((s = fabs(xy.y *= RC2)) < 1.) {
		c = cos(s = asin(xy.y));
		lp.phi = asin((s + xy.y * (c + 2.)) * RC3);
		lp.lam = xy.x / (C1 * (1. + c));
	} else if ((s - EPS10) > 1. ) I_ERROR
	else {
		lp.lam = 0.;
		lp.phi = xy.y < 0. ? - HALFPI : HALFPI;
	}
	return (lp);
}
FREEUP {  if (P) free(P); }
ENTRY(pj_eck4) {
	if (!P)
		return((PJ *)malloc(sizeof(PJ)));
	P->inv = s_inverse;
	P->fwd = s_forward;
	P->pfree = freeup;
	return P;
}
