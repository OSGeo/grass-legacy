/*  Van der Grinten II Projection */
#ifndef lint
static char RCSID[] = "@(#)$Id: PJ_vandg2.c,v 4.2 1992/07/14 01:28:52 gie Exp $";
#endif
# define TOL	1e-10
# define TWORPI	0.63661977236758134308
#define __PROJ_PARMS \
	int	vdg3;
#define __PJ_LIB
#include	"projects.h"
FORWARD(s_forward) { XY xy;  /* spheroid */
	double x1, at, bt, ct;

	bt = fabs(TWORPI * lp.phi);
	if ((ct = 1. - bt * bt) < 0.)
		ct = 0.;
	else
		ct = sqrt(ct);
	if (fabs(lp.lam) < TOL) {
		xy.x = 0.;
		xy.y = PI * (lp.phi < 0. ? -bt : bt) / (1. + ct);
	} else {
		at = 0.5 * fabs(PI / lp.lam - lp.lam / PI);
		if (P->vdg3) {
			x1 = bt / (1. + ct);
			xy.x = PI * (sqrt(at * at + 1. - x1 * x1) - at);
			xy.y = PI * x1;
		} else {
			x1 = (ct * sqrt(1. + at * at) - at * ct * ct) /
				(1. + at * at * bt * bt);
			xy.x = PI * x1;
			xy.y = PI * sqrt(1. - x1 * (x1 + 2. * at) + TOL);
		}
		if ( lp.lam < 0.) xy.x = -xy.x;
		if ( lp.phi < 0.) xy.y = -xy.y;
	}
	return (xy);
}
FREEUP {  if (P) free(P); }
ENTRY(pj_vandg2) {
	if (!P)
		return((PJ *)malloc(sizeof(PJ)));
	P->vdg3 = 0;
	P->inv = 0; P->fwd = s_forward; P->pfree = freeup;
	return P;
}
ENTRY(pj_vandg3) {
	if (!P)
		return((PJ *)malloc(sizeof(PJ)));
	P->vdg3 = 1;
	P->inv = 0; P->fwd = s_forward; P->pfree = freeup;
	return P;
}
