/*  Quartic Authalic */
#ifndef lint
static char RCSID[] = "@(#)$Id: PJ_quau.c,v 4.2 1992/07/14 01:27:51 gie Exp $";
#endif
#define __PJ_LIB
# include	"projects.h"
#define ONEEPS	1.0000001
FORWARD(s_forward) { XY xy;  /* spheroid */
	xy.x = lp.lam * cos(lp.phi);
	xy.x /= cos(lp.phi *= 0.5);
	xy.y = 2. * sin(lp.phi);
	return (xy);
}
INVERSE(s_inverse) { LP lp;  /* spheroid */
	if (fabs(xy.y *= .5) >= 1.)
		if (fabs(xy.y) > ONEEPS)	I_ERROR
		else		lp.phi = xy.y < 0. ? PI : -PI;
	else
		lp.phi = 2. * asin(xy.y);
	if ((lp.lam = cos(lp.phi)) == 0.)
		lp.lam = 0.;
	else
		lp.lam = xy.x * cos(.5 * lp.phi) / lp.lam;
	return (lp);
}
FREEUP {  if (P) free(P); }
ENTRY(pj_quau) {
	if (!P)
		return((PJ *)malloc(sizeof(PJ)));
	P->inv = s_inverse; P->fwd = s_forward; P->pfree = freeup;
	return P;
}
