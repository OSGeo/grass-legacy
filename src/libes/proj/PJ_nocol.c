/*  Nicolosi Globular */
#ifndef lint
static char RCSID[] = "@(#)$Id: PJ_nocol.c,v 4.2 1992/07/14 01:27:43 gie Exp $";
#endif
#define __PJ_LIB
#include	"projects.h"
#define EPS	1e-10
FORWARD(s_forward) { XY xy;  /* spheroid */
	if (fabs(lp.lam) < EPS) {
		xy.x = 0;
		xy.y = lp.phi;
	} else if (fabs(lp.phi) < EPS) {
		xy.x = lp.lam;
		xy.y = 0.;
	} else if (fabs(fabs(lp.lam) - HALFPI) < EPS) {
		xy.x = lp.lam * cos(lp.phi);
		xy.y = HALFPI * sin(lp.phi);
	} else if (fabs(fabs(lp.phi) - HALFPI) < EPS) {
		xy.x = 0;
		xy.y = lp.phi;
	} else {
		double tb, c, d, m, n, r2, sp;

		tb = HALFPI / lp.lam - lp.lam / HALFPI;
		c = lp.phi / HALFPI;
		d = (1 - c * c)/((sp = sin(lp.phi)) - c);
		r2 = tb / d;
		r2 *= r2;
		m = (tb * sp / d - 0.5 * tb)/(1. + r2);
		n = (sp / r2 + 0.5 * d)/(1. + 1./r2);
		xy.x = cos(lp.phi);
		xy.x = sqrt(m * m + xy.x * xy.x / (1. + r2));
		xy.x = HALFPI * ( m + (lp.lam < 0. ? -xy.x : xy.x));
		xy.y = sqrt(n * n - (sp * sp / r2 + d * sp - 1.) /
			(1. + 1./r2));
		xy.y = HALFPI * ( n + (lp.phi < 0. ? xy.y : -xy.y ));
	}
	return (xy);
}
FREEUP {  if (P) free(P); }
ENTRY(pj_nicol) {
	if (!P)
		return((PJ *)malloc(sizeof(PJ)));
	P->inv = 0;
	P->fwd = s_forward;
	P->pfree = freeup;
	return P;
}
