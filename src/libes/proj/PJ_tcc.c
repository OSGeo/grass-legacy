/* Transverse Central Cylindrical projection */
#ifndef lint
static char RCSID[] = "@(#)$Id: PJ_tcc.c,v 4.2 1992/07/14 01:28:47 gie Exp $";
#endif
#define __PROJ_PARMS \
	double ap;
#define EPS10 1.e-10
#define __PJ_LIB
#include	"projects.h"
FORWARD(s_forward) { XY xy;  /* spheroid */
	double b, bt;

	b = cos(lp.phi) * sin(lp.lam);
	if ((bt = 1. - b * b) < EPS10) F_ERROR;
	xy.x = b / sqrt(bt);
	xy.y = atan2(tan(lp.phi) , cos(lp.lam));
	return (xy);
}
FREEUP {  if (P) free(P); }
ENTRY(pj_tcc) {
	if (!P)
		return((PJ *)malloc(sizeof(PJ)));
	P->inv = 0;
	P->fwd = s_forward;
	P->pfree = freeup;
	return P;
}
