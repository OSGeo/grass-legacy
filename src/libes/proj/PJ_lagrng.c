/*  Lagrange Projection */
#ifndef lint
static char RCSID[] = "@(#)$Id: PJ_lagrng.c,v 4.3 1992/07/14 01:27:32 gie Exp $";
#endif
#define __PROJ_PARMS \
	double	hrw; \
	double	rw; \
	double	a1;
#define TOL	1e-10
#define __PJ_LIB
#include	"projects.h"
FORWARD(s_forward) { XY xy;  /* spheroid */
	double v, c;

	if (fabs(fabs(lp.phi) - HALFPI) < TOL) {
		xy.x = 0;
		xy.y = lp.phi < 0 ? -2. : 2.;
	} else {
		lp.phi = sin(lp.phi);
		v = P->a1 * pow((1. + lp.phi)/(1. - lp.phi), P->hrw);
		if ((c = 0.5 * (v + 1./v) + cos(lp.lam *= P->rw)) < TOL)
			F_ERROR;
		xy.x = 2. * sin(lp.lam) / c;
		xy.y = (v - 1./v) / c;
	}
	return (xy);
}
FREEUP {  if (P) free(P); }
ENTRY(pj_lagrng) {
	double phi1;

	if (!P)
		return((PJ *)malloc(sizeof(PJ)));
	if ((P->rw = pj_param("dW", "2")->f) <= 0)
		emess(1,"W <= 0");
	P->hrw = 0.5 * (P->rw = 1. / P->rw);
	phi1 = pj_param("rlat_1", "0")->f;
	if (fabs(fabs(phi1 = sin(phi1)) - 1.) < TOL)
		emess(1,"|phi_1| == 90");
	P->a1 = pow((1. - phi1)/(1. + phi1), P->hrw);
	P->inv = 0; P->fwd = s_forward; P->pfree = freeup;
	return P;
}
