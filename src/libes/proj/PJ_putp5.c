/*  Putnins P5 Projection */
#ifndef lint
static char RCSID[] = "@(#)$Id: PJ_putp5.c,v 4.2 1992/07/14 01:27:50 gie Exp $";
#endif
#define __PJ_LIB
#include	"projects.h"
#define FXC	1.01346
#define FYC	1.01346
#define CS	1.21585420370805325734
FORWARD(s_forward) { XY xy;  /* spheroid */
	xy.x = FXC * lp.lam * (2. - sqrt(1. + CS * lp.phi * lp.phi));
	xy.y = FYC * lp.phi;
	return (xy);
}
INVERSE(s_inverse) { LP lp;  /* spheroid */
	lp.phi = xy.y / FYC;
	lp.lam = xy.x / (FXC * (2. - sqrt(1. + CS * lp.phi * lp.phi)));
	return (lp);
}
FREEUP {  if (P) free(P); }
ENTRY(pj_putp5) {
	if (!P)
		return((PJ *)malloc(sizeof(PJ)));
	P->inv = s_inverse; P->fwd = s_forward; P->pfree = freeup;
	return P;
}
