/* Equal Area Cylindrical projection */
#ifndef lint
static char RCSID[] = "@(#)$Id: PJ_cea.c,v 4.2 1992/07/14 01:27:14 gie Exp $";
#endif
#define __PROJ_PARMS \
	double k0; \
	double qp; \
	double *apa;
#define __PJ_LIB
# include	"projects.h"
# define EPS	1e-10
FORWARD(e_forward) { XY xy;  /* spheroid */
	xy.x = P->k0 * lp.lam;
	xy.y = .5 * pj_qsfn(sin(lp.phi), P->e, P->one_es) / P->k0;
	return (xy);
}
FORWARD(s_forward) { XY xy;  /* spheroid */
	xy.x = P->k0 * lp.lam;
	xy.y = sin(lp.phi) / P->k0;
	return (xy);
}
INVERSE(e_inverse) { LP lp;  /* spheroid */
	lp.phi = pj_authlat(asin( 2. * xy.y * P->k0 / P->qp), P->apa);
	lp.lam = xy.x / P->k0;
	return (lp);
}
INVERSE(s_inverse) { LP lp;  /* spheroid */
	double t;

	if ((t = fabs(xy.y *= P->k0)) - EPS <= 1.) {
		if (t >= 1.)
			lp.phi = xy.y < 0. ? -HALFPI : HALFPI;
		else
			lp.phi = asin(xy.y);
		lp.lam = xy.x / P->k0;
	} else I_ERROR;
	return (lp);
}
FREEUP { 
	if (P) {
		if (P->apa)
			free(P->apa);
		free(P);
	}
}
ENTRY(pj_cea) {
	double t;

	if (!P)
		return((PJ *)malloc(sizeof(PJ)));
	if ((P->k0 = cos(t = pj_param("rlat_ts", "")->f)) < 0.) {
		emess(-1,"lat_ts >= 90");
		E_ERROR;
	}
	if (P->es) {
		t = sin(t);
		P->k0 /= sqrt(1. - P->es * t * t);
		P->e = sqrt(P->es);
		if (!(P->apa = pj_authset(P->es))) {
			emess(-1,"failed to allocate authalic memory");
			E_ERROR;
		}
		P->qp = pj_qsfn(1., P->e, P->one_es);
		P->inv = e_inverse;
		P->fwd = e_forward;
	} else {
		P->inv = s_inverse;
		P->fwd = s_forward;
	}
	P->pfree = freeup;
}
