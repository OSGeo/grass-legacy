/*  Hammer & Eckert-Greifendorff Projections */
#ifndef lint
static char RCSID[] = "@(#)$Id: PJ_hammer.c,v 4.2 1992/07/14 01:27:29 gie Exp $";
#endif
#define __PROJ_PARMS \
	double w;
#define __PJ_LIB
# include	"projects.h"
FORWARD(s_forward) { XY xy;  /* spheroid */
	double cosphi, d;

	d = sqrt(2./(1. + (cosphi = cos(lp.phi)) * cos(lp.lam *= P->w)));
	xy.x = d * cosphi * sin(lp.lam) / P->w;
	xy.y = d * sin(lp.phi);
	return (xy);
}
FREEUP {  if (P) free(P); }
ENTRY(pj_hammer) {
	if (!P)
		return((PJ *)malloc(sizeof(PJ)));
	if ((P->w = fabs(pj_param("dW", "0.5")->f)) == 0.) {
		emess(1,"W == 0.");
		E_ERROR;
	}
	P->inv = 0; P->fwd = s_forward; P->pfree = freeup;
	return P;
}
