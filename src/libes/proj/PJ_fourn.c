/*  Fournier Globular I */
#ifndef lint
static char RCSID[] = "@(#)$Id: PJ_fourn.c,v 4.2 1992/07/14 01:27:25 gie Exp $";
#endif
#define __PJ_LIB
#include	"projects.h"
#define EPS	1e-10
#define C	2.46740110027233965467
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
	} else {
		double p, s, at;

		p = fabs(PI * sin(lp.phi));
		s = (C - lp.phi * lp.phi)/(p - 2. * fabs(lp.phi));
		at = lp.lam * lp.lam / C - 1.;
		if ((xy.y = s * s - at * (C - p * s - lp.lam * lp.lam)) < 0.) {
			if (xy.y < -EPS) F_ERROR
			else xy.y = -s / at;
		} else
			xy.y = (sqrt(xy.y) - s) / at;
		if (lp.phi < 0.) xy.y = -xy.y;
		if ((xy.x = 1. - xy.y * xy.y / C) < 0.) {
			if (xy.x < -EPS) F_ERROR
			else xy.x = 0;
		} else
			xy.x = lp.lam * sqrt(xy.x);
	}
	return (xy);
}
FREEUP {  if (P) free(P); }
ENTRY(pj_fourn) {
	if (!P)
		return((PJ *)malloc(sizeof(PJ)));
	P->inv = 0;
	P->fwd = s_forward;
	P->pfree = freeup;
	return P;
}
