/* Mollweide projection */
#ifndef lint
static char RCSID[] = "@(#)$Id: PJ_moll.c,v 4.2 1992/07/14 01:27:42 gie Exp $";
#endif
#define __PJ_LIB
#include	"projects.h"
#define C1R	.90031631615710606956
#define C2R	1.41421356237309504880
#define EPS	1e-15
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
	double th, dth;
	int i;

	ph = PI * sin(th = ph);
	for (i = NITER; i ; --i) {
		th += ( dth = (ph - th - sin(th)) / (1. + cos(th)) );
		if (fabs(dth) < EPS)
			break;
	}
	return (.5 * th);
}
FORWARD(s_forward) { XY xy;  /* spheroid */
	double th;

	xy.x = C1R * lp.lam * cos(th = theta(lp.phi));
	xy.y = C2R * sin(th);
	return (xy);
}
INVERSE(s_inverse) { LP lp;  /* spheroid */
	double th, s;

	if ((s = fabs(th = xy.y / C2R)) < 1.) {
		lp.lam = xy.x / (C1R * cos(th = asin(th)));
		th += th;
		lp.phi = asin((th + sin(th)) / PI);
	} else if ((s - EPS10) > 1.)
		lp.lam = lp.phi = HUGE_VAL;
	else {
		lp.lam = 0.;
		lp.phi = th < 0. ? -HALFPI : HALFPI;
	}
	return (lp);
}
FREEUP {  if (P) free(P); }
ENTRY(pj_moll) {
	if (!P)
		return((PJ *)malloc(sizeof(PJ)));
	P->inv = s_inverse; P->fwd = s_forward; P->pfree = freeup;
	return P;
}
