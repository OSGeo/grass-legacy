/*  Denoyer Semi-Elliptical */
#ifndef lint
static char RCSID[] = "@(#)$Id: PJ_dense.c,v 4.2 1992/07/14 01:27:16 gie Exp $";
#endif
#define __PJ_LIB
#include	"projects.h"
#define C0	0.95
#define C1	-.08333333333333333333
#define C3	.00166666666666666666
#define D1	0.9
#define D5	0.03
FORWARD(s_forward) { XY xy;  /* spheroid */
	xy.y = lp.phi;
	xy.x = lp.lam;
	lp.lam = fabs(lp.lam);
	xy.x *= cos((C0 + lp.lam * (C1 + lp.lam * lp.lam * C3)) *
		(lp.phi * (D1 + D5 * lp.phi * lp.phi * lp.phi * lp.phi)));
	return (xy);
}
FREEUP {  if (P) free(P); }
ENTRY(pj_dense) {
	if (!P)
		return((PJ *)malloc(sizeof(PJ)));
	P->inv = 0;
	P->fwd = s_forward;
	P->pfree = freeup;
	return P;
}
