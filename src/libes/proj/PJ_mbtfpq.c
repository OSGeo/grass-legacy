/*  McBryde-Thomas Flat-Polar Quartic */
#ifndef lint
static char RCSID[] = "@(#)$Id: PJ_mbtfpq.c,v 4.2 1992/07/14 01:27:37 gie Exp $";
#endif
#define __PJ_LIB
#include	"projects.h"
#define NITER	20
#define EPS	1e-7
#define ONETOL 1.000001
#define C	1.70710678118654752440
#define RC	0.58578643762690495119
#define FYC	1.87475828462269495505
#define RYC	0.53340209679417701685
#define FXC	0.31245971410378249250
#define RXC	3.20041258076506210122
FORWARD(s_forward) { XY xy;  /* spheroid */
	double th1, c;
	int i;

	c = C * sin(lp.phi);
	for (i = NITER; i; --i) {
		lp.phi -= th1 = (sin(.5*lp.phi) + sin(lp.phi) - c) /
			(.5*cos(.5*lp.phi)  + cos(lp.phi));
		if (fabs(th1) < EPS) break;
	}
	xy.x = FXC * lp.lam * (1.0 + 2. * cos(lp.phi)/cos(0.5 * lp.phi));
	xy.y = FYC * sin(0.5 * lp.phi);
	return (xy);
}
INVERSE(s_inverse) { LP lp;  /* spheroid */
	double t;

	lp.phi = RYC * xy.y;
	if (fabs(lp.phi) > 1.) {
		if (fabs(lp.phi) > ONETOL)	I_ERROR
		else if (lp.phi < 0.) { t = -1.; lp.phi = -PI; }
		else { t = 1.; lp.phi = PI; }
	} else
		lp.phi = 2. * asin(t = lp.phi);
	lp.lam = RXC * xy.x / (1. + 2. * cos(lp.phi)/cos(0.5 * lp.phi));
	lp.phi = RC * (t + sin(lp.phi));
	if (fabs(lp.phi) > 1.)
		if (fabs(lp.phi) > ONETOL)	I_ERROR
		else			lp.phi = lp.phi < 0. ? -HALFPI : HALFPI;
	else
		lp.phi = asin(lp.phi);
	return (lp);
}
FREEUP {  if (P) free(P); }
ENTRY(pj_mbtfpq) {
	if (!P)
		return((PJ *)malloc(sizeof(PJ)));
	P->inv = s_inverse; P->fwd = s_forward; P->pfree = freeup;
	return P;
}
