/* Sinusoidal Projection */
#ifndef lint
static char RCSID[] = "@(#)$Id: PJ_sinu.c,v 4.2 1992/07/14 01:27:54 gie Exp $";
#endif
#define __PROJ_PARMS \
	double	*en;
#define __PJ_LIB
#include	"projects.h"
#define EPS10	1e-10
FORWARD(e_forward) { XY xy;  /* ellipsoid */
	double s, c;

	xy.y = pj_mlfn(lp.phi, s = sin(lp.phi), c = cos(lp.phi), P->en);
	xy.x = lp.lam * c / sqrt(1. - P->es * s * s);
	return (xy);
}
FORWARD(s_forward) { XY xy;  /* sphere */
	xy.x = lp.lam * cos(xy.y = lp.phi);
	return (xy);
}
INVERSE(e_inverse) { LP lp;  /* ellipsoid */
	double s;

	if ((s = fabs(lp.phi = pj_inv_mlfn(xy.y, P->es, P->en))) < HALFPI) {
		s = sin(lp.phi);
		lp.lam = xy.x * sqrt(1. - P->es * s * s) / cos(lp.phi);
	} else if ((s - EPS10) < HALFPI)
		lp.lam = 0.;
	else I_ERROR;
	return (lp);
}
INVERSE(s_inverse) { LP lp;  /* sphere */
	double s;

	if ((s = fabs(lp.phi = xy.y)) < HALFPI)
		lp.lam = xy.x / cos(lp.phi);
	else if ((s - EPS10) < HALFPI)
		lp.lam = 0.;
	else I_ERROR;
	return (lp);
}
FREEUP {  if (P) { if (P->en) free(P->en); free(P); } }
ENTRY(pj_sinu) {
	if (!P)
		return((PJ *)malloc(sizeof(PJ)));
	if (!(P->en = pj_enfn(P->es)))
		E_ERROR;
	if (P->es) {
		P->en = pj_enfn(P->es);
		P->inv = e_inverse;
		P->fwd = e_forward;
	} else {
		P->inv = s_inverse;
		P->fwd = s_forward;
	}
	P->pfree = freeup;
	return P;
}
