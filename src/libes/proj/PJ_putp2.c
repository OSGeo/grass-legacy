/*  Putnins P2' */
#ifndef lint
static char RCSID[] = "@(#)$Id: PJ_putp2.c,v 4.3 1992/07/14 01:27:49 gie Exp $";
#endif
#define __PJ_LIB
#include	"projects.h"
#define NITER	20
#define EPS	1e-7
#define ONETOL 1.000001
#define C	2.96042050617763413905
#define RC	0.33778985043282124554
#define FYC	1.56548
#define RYC	0.63878171551217517949
#define FXC	0.86310
#define RXC	1.15861429730042868729
FORWARD(s_forward) { XY xy;  /* spheroid */
	double th1, c;
	int i;

	c = C * sin(lp.phi);
	for (i = NITER; i; --i) {
		lp.phi -= th1 = (lp.phi + sin(lp.phi) - c) / (1. + cos(lp.phi));
		if (fabs(th1) < EPS) break;
	}
	xy.x = FXC * lp.lam * cos(lp.phi *= 0.5);
	xy.y = FYC * sin(lp.phi);
	return (xy);
}
INVERSE(s_inverse) { LP lp;  /* spheroid */
	lp.phi = RYC * xy.y;
	if (fabs(lp.phi) > 1.)
		if (fabs(lp.phi) > ONETOL)	I_ERROR
		else lp.phi = lp.phi > 0. ? HALFPI : - HALFPI;
	else
		lp.phi = asin(lp.phi);
	lp.lam = RXC * xy.x / cos(lp.phi);
	lp.phi += lp.phi;
	lp.phi = RC * (lp.phi + sin(lp.phi));
	if (fabs(lp.phi) > 1.)
		if (fabs(lp.phi) > ONETOL)	I_ERROR
		else lp.phi = lp.phi > 0. ? HALFPI : - HALFPI;
	else
		lp.phi = asin(lp.phi);
	return (lp);
}
FREEUP {  if (P) free(P); }
ENTRY(pj_putp2) {
	if (!P)
		return((PJ *)malloc(sizeof(PJ)));
	P->inv = s_inverse; P->fwd = s_forward; P->pfree = freeup;
	return P;
}
