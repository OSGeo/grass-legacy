/* Polyconic (American) projection */
#ifndef lint
static char RCSID[] = "@(#)$Id: PJ_poly.c,v 4.3 1992/07/14 01:27:49 gie Exp $";
#endif
#define __PROJ_PARMS \
	double ml0; \
	double *en;
#define __PJ_LIB
#include "projects.h"
#define TOL	1e-10
#define CONV	1e-10
#define N_ITER	10
#define I_ITER 20
#define ITOL 1.e-12
FORWARD(e_forward) { XY xy;  /* ellipsoid */
	double  ms, sp, cp;

	if (fabs(lp.phi) <= TOL) { xy.x = lp.lam; xy.y = -P->ml0; }
	else {
		sp = sin(lp.phi);
		ms = fabs(cp = cos(lp.phi)) > TOL ? pj_msfn(sp, cp, P->es) / sp : 0.;
		xy.x = ms * sin(lp.lam *= sp);
		xy.y = (pj_mlfn(lp.phi, sp, cp, P->en) - P->ml0) + ms * (1. - cos(lp.lam));
	}
	return (xy);
}
FORWARD(s_forward) { XY xy;  /* spheroid */
	double  cot, E;

	if (fabs(lp.phi) <= TOL) { xy.x = lp.lam; xy.y = P->ml0; }
	else {
		cot = 1. / tan(lp.phi);
		xy.x = sin(E = lp.lam * sin(lp.phi)) * cot;
		xy.y = lp.phi - P->phi0 + cot * (1. - cos(E));
	}
	return (xy);
}
INVERSE(e_inverse) { LP lp;  /* ellipsoid */
	xy.y += P->ml0;
	if (fabs(xy.y) <= TOL) { lp.lam = xy.x; lp.phi = 0.; }
	else {
		double r, c, sp, cp, s2ph, ml, mlb, mlp, dPhi;
		int i;

		r = xy.y * xy.y + xy.x * xy.x;
		for (lp.phi = xy.y, i = I_ITER; i ; --i) {
			sp = sin(lp.phi);
			s2ph = sp * ( cp = cos(lp.phi));
			if (fabs(cp) < ITOL)
				I_ERROR;
			c = sp * (mlp = sqrt(1. - P->es * sp * sp)) / cp;
			ml = pj_mlfn(lp.phi, sp, cp, P->en);
			mlb = ml * ml + r;
			mlp = P->one_es / (mlp * mlp * mlp);
			lp.phi += ( dPhi =
				( ml + ml + c * mlb - 2. * xy.y * (c * ml + 1.) ) / (
				P->es * s2ph * (mlb - 2. * xy.y * ml) / c +
				2.* (xy.y - ml) * (c * mlp - 1. / s2ph) - mlp - mlp ));
			if (fabs(dPhi) <= ITOL)
				break;
		}
		if (!i)
			I_ERROR;
		c = sin(lp.phi);
		lp.lam = asin(xy.x * tan(lp.phi) * sqrt(1. - P->es * c * c)) / sin(lp.phi);
	}
	return (lp);
}
INVERSE(s_inverse) { LP lp;  /* spheroid */
	double B, dphi, tp;
	int i;

	if (fabs(xy.y = P->phi0 + xy.y) <= TOL) { lp.lam = xy.x; lp.phi = 0.; }
	else {
		lp.phi = xy.y;
		B = xy.x * xy.x + xy.y * xy.y;
		i = N_ITER;
		do {
			tp = tan(lp.phi);
			lp.phi -= (dphi = (xy.y * (lp.phi * tp + 1.) - lp.phi -
				.5 * ( lp.phi * lp.phi + B) * tp) /
				((lp.phi - xy.y) / tp - 1.));
		} while (fabs(dphi) > CONV && --i);
		if (! i) I_ERROR;
		lp.lam = asin(xy.x * tan(lp.phi)) / sin(lp.phi);
	}
	return (lp);
}
FREEUP {  if (P) { if (P->en) free(P->en); free(P); } }
ENTRY(pj_poly) {
	if (!P)
		return((PJ *)malloc(sizeof(PJ)));
	if (P->es) {
		if (!(P->en = pj_enfn(P->es)))
			E_ERROR;
		P->ml0 = pj_mlfn(P->phi0, sin(P->phi0), cos(P->phi0), P->en);
		P->inv = e_inverse;
		P->fwd = e_forward;
	} else {
		P->ml0 = -P->phi0;
		P->inv = s_inverse;
		P->fwd = s_forward;
	}
	P->pfree = freeup;
	return P;
}
