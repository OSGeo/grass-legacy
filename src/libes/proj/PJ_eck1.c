/*  Eckert I Projection */
#ifndef lint
static char RCSID[] = "@(#)$Id: PJ_eck1.c,v 4.2 1992/07/14 01:27:17 gie Exp $";
#endif
#define __PJ_LIB
#include	"projects.h"
#define FC	.92131773192356127802
#define RP	.31830988618379067154
FORWARD(s_forward) { XY xy;  /* spheroid */
	xy.x = FC * lp.lam * (1. - RP * fabs(lp.phi));
	xy.y = FC * lp.phi;
	return (xy);
}
INVERSE(s_inverse) { LP lp;  /* spheroid */
	lp.phi = xy.y / FC;
	lp.lam = xy.x / (FC * (1. - RP * fabs(lp.phi)));
	return (lp);
}
FREEUP {  if (P) free(P); }
ENTRY(pj_eck1) {
	if (!P)
		return((PJ *)malloc(sizeof(PJ)));
	P->inv = s_inverse; P->fwd = s_forward; P->pfree = freeup;
	return P;
}
