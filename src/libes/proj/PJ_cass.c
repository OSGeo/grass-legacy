/* Cassini projection */
#ifndef lint
static char RCSID[] = "@(#)$Id: PJ_cass.c,v 4.3 1992/07/14 01:27:11 gie Exp $";
#endif
#define __PROJ_PARMS \
	double m0; \
	double n; \
	double t; \
	double a1; \
	double c; \
	double r; \
	double dd; \
	double d2; \
	double a2; \
	double tn; \
	double *en;
#define __PJ_LIB
# include	"projects.h"
# define EPS10	1e-10
# define C1	.16666666666666666666
# define C2	.00833333333333333333
# define C3	.04166666666666666666
# define C4	.33333333333333333333
# define C5	.06666666666666666666
FORWARD(e_forward) { XY xy;  /* ellipsoid */
	xy.y = pj_mlfn(lp.phi, P->n = sin(lp.phi), P->c = cos(lp.phi), P->en);
	P->n = 1./sqrt(1. - P->es * P->n * P->n);
	P->tn = tan(lp.phi); P->t = P->tn * P->tn;
	P->a1 = lp.lam * P->c;
	P->c *= P->es * P->c / (1 - P->es);
	P->a2 = P->a1 * P->a1;
	xy.x = P->n * P->a1 * (1. - P->a2 * P->t *
		(C1 - (8. - P->t + 8. * P->c) * P->a2 * C2));
	xy.y -= P->m0 - P->n * P->tn * P->a2 *
		(.5 + (5. - P->t + 6. * P->c) * P->a2 * C3);
	return (xy);
}
FORWARD(s_forward) { XY xy;  /* spheroid */
	xy.x = asin(cos(lp.phi) * sin(lp.lam));
	xy.y = atan2(tan(lp.phi) , cos(lp.lam)) - P->phi0;
	return (xy);
}
INVERSE(e_inverse) { LP lp;  /* ellipsoid */
	double ph1;

	ph1 = pj_inv_mlfn(P->m0 + xy.y, P->es, P->en);
	P->tn = tan(ph1); P->t = P->tn * P->tn;
	P->n = sin(ph1);
	P->r = 1. / (1. - P->es * P->n * P->n);
	P->n = sqrt(P->r);
	P->r *= (1. - P->es) * P->n;
	P->dd = xy.x / P->n;
	P->d2 = P->dd * P->dd;
	lp.phi = ph1 - (P->n * P->tn / P->r) * P->d2 *
		(.5 - (1. + 3. * P->t) * P->d2 * C3);
	lp.lam = P->dd * (1. + P->t * P->d2 *
		(-C4 + (1. + 3. * P->t) * P->d2 * C5)) / cos(ph1);
	return (lp);
}
INVERSE(s_inverse) { LP lp;  /* spheroid */
	lp.phi = asin(sin(P->dd = xy.y + P->phi0) * cos(xy.x));
	lp.lam = atan2(tan(xy.x), cos(P->dd));
	return (lp);
}
FREEUP { 
	if (P) {
		if (P->en)
			free(P->en);
		free(P);
	}
}
ENTRY(pj_cass) {
	if (!P)
		return((PJ *)malloc(sizeof(PJ)));
	if (P->es) {
		if (!(P->en = pj_enfn(P->es)))
			E_ERROR;
		P->m0 = pj_mlfn(P->phi0, sin(P->phi0), cos(P->phi0), P->en);
		P->inv = e_inverse;
		P->fwd = e_forward;
	} else {
		P->inv = s_inverse;
		P->fwd = s_forward;
	}
	P->pfree = freeup;
	return P;
}
