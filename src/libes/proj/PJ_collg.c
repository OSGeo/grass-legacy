/*  Collignon projection */
#ifndef lint
static char RCSID[] = "@(#)$Id: PJ_collg.c,v 4.2 1992/07/14 01:27:15 gie Exp $";
#endif
#define __PJ_LIB
# include	"projects.h"
#define FXC	1.12837916709551257390
#define FYC	1.77245385090551602729
#define ONEEPS	1.0000001
FORWARD(s_forward) { XY xy;  /* spheroid */
	if ((xy.y = 1. - sin(lp.phi)) <= 0.)
		xy.y = 0.;
	else
		xy.y = sqrt(xy.y);
	xy.x = FXC * lp.lam * xy.y;
	xy.y = FYC * (1. - xy.y);
	return (xy);
}
INVERSE(s_inverse) { LP lp;  /* spheroid */
	lp.phi = xy.y / FYC - 1.;
	if (fabs(lp.phi = 1. - lp.phi * lp.phi) < 1.)
		lp.phi = asin(lp.phi);
	else if (fabs(lp.phi) > ONEEPS) I_ERROR
	else	lp.phi = lp.phi < 0. ? -HALFPI : HALFPI;
	if ((lp.lam = 1. - sin(lp.phi)) <= 0.)
		lp.lam = 0.;
	else
		lp.lam = xy.x / (FXC * sqrt(lp.lam));
	return (lp);
}
FREEUP {  if (P) free(P); }
ENTRY(pj_collg) {
	if (!P)
		P = (PJ *)malloc(sizeof(PJ));
	else {
		P->inv = s_inverse;
		P->fwd = s_forward;
		P->pfree = freeup;
	}
	return P;
}
