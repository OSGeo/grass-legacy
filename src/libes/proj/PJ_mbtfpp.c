/*  McBride-Thomas Flat-Polar Parabolic */
#ifndef lint
static char RCSID[] = "@(#)$Id: PJ_mbtfpp.c,v 4.2 1992/07/14 01:27:36 gie Exp $";
#endif
#define __PJ_LIB
#include	"projects.h"
#define CS	.95257934441568037152
#define FXC	.92582009977255146156
#define FYC	3.40168025708304504493
#define C23	.66666666666666666666
#define C13	.33333333333333333333
#define ONEEPS	1.0000001
FORWARD(s_forward) { XY xy;  /* spheroid */
	lp.phi = asin(CS * sin(lp.phi));
	xy.x = FXC * lp.lam * (2. * cos(C23 * lp.phi) - 1.);
	xy.y = FYC * sin(C13 * lp.phi);
	return (xy);
}
INVERSE(s_inverse) { LP lp;  /* spheroid */
	lp.phi = xy.y / FYC;
	if (fabs(lp.phi) >= 1.) {
		if (fabs(lp.phi) > ONEEPS)	I_ERROR
		else	lp.phi = (lp.phi < 0.) ? -HALFPI : HALFPI;
	} else
		lp.phi = asin(lp.phi);
	lp.lam = xy.x / ( FXC * (2. * cos(C23 * (lp.phi *= 3.)) - 1.) );
	if (fabs(lp.phi = sin(lp.phi) / CS) >= 1.) {
		if (fabs(lp.phi) > ONEEPS)	I_ERROR
		else	lp.phi = (lp.phi < 0.) ? -HALFPI : HALFPI;
	} else
		lp.phi = asin(lp.phi);
	return (lp);
}
FREEUP {  if (P) free(P); }
ENTRY(pj_mbtfpp) {
	if (!P)
		return((PJ *)malloc(sizeof(PJ)));
	P->inv = s_inverse; P->fwd = s_forward; P->pfree = freeup;
	return P;
}
