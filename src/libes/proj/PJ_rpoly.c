/*  Rectangular Polyconic */
#ifndef lint
static char RCSID[] = "@(#)$Id: PJ_rpoly.c,v 4.2 1992/07/14 01:27:53 gie Exp $";
#endif
#define __PROJ_PARMS \
	double	phi1; \
	double	fxa; \
	double	fxb; \
	int		mode;
#define EPS	1e-9
#define __PJ_LIB
#include	"projects.h"
FORWARD(s_forward) { XY xy;  /* spheroid */
	double fa;

	if (P->mode)
		fa = tan(lp.lam * P->fxb) * P->fxa;
	else
		fa = 0.5 * lp.lam;
	if (fabs(lp.phi) < EPS) {
		xy.x = fa + fa;
		xy.y = - P->phi0;
	} else {
		xy.y = 1. / tan(lp.phi);
		xy.x = sin(fa = 2. * atan(fa * sin(lp.phi))) * xy.y;
		xy.y = lp.phi - P->phi0 + (1. - cos(fa)) * xy.y;
	}
	return (xy);
}
FREEUP {  if (P) free(P); }
ENTRY(pj_rpoly) {
	if (!P)
		return((PJ *)malloc(sizeof(PJ)));
	if ((P->mode = (P->phi1 = fabs(pj_param("rlat_ts","0")->f)) > EPS)) {
		P->fxb = 0.5 * sin(P->phi1);
		P->fxa = 0.5 / P->fxb;
	}
	P->inv = 0; P->fwd = s_forward; P->pfree = freeup;
	return P;
}
