/* Central Cylindrical projection */
#ifndef lint
static char RCSID[] = "@(#)$Id: PJ_cc.c,v 4.2 1992/07/14 01:27:12 gie Exp $";
#endif
#define __PROJ_PARMS \
	double ap;
#define __PJ_LIB
#include	"projects.h"
#define EPS10 1.e-10
FORWARD(s_forward) { XY xy;  /* spheroid */
	if (fabs(fabs(lp.phi) - HALFPI) <= EPS10) F_ERROR;
	xy.x = lp.lam;
	xy.y = tan(lp.phi);
	return (xy);
}
INVERSE(s_inverse) { LP lp;  /* spheroid */
	lp.phi = atan(xy.y);
	lp.lam = xy.x;
	return (lp);
}
FREEUP {  if (P) free(P); }
ENTRY(pj_cc) {
	if (!P)
		return((PJ *)malloc(sizeof(PJ)));
	P->inv = s_inverse;
	P->fwd = s_forward;
	P->pfree = freeup;
	return P;
}
