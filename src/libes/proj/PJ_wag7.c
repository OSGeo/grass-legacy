/*  Wagner VII Projection */
#ifndef lint
static char RCSID[] = "@(#)$Id: PJ_wag7.c,v 4.2 1992/07/14 01:28:54 gie Exp $";
#endif
#define __PJ_LIB
#include	"projects.h"
#define THIRD	0.3333333333333333333
FORWARD(s_forward) { XY xy;  /* spheroid */
	double s, c0, c1;

	s = 0.90631 * sin(lp.phi);
	c0 = sqrt(1. - s * s);
	c1 = sqrt(2./(1. + c0 * cos(lp.lam *= THIRD)));
	xy.x = 2.66723 * c0 * c1 * sin(lp.lam);
	xy.y = 1.24104 * s * c1;
	return (xy);
}
FREEUP {  if (P) free(P); }
ENTRY(pj_wag7) {
	if (!P)
		return((PJ *)malloc(sizeof(PJ)));
	P->inv = 0;
	P->fwd = s_forward;
	P->pfree = freeup;
	return P;
}
