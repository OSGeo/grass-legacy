/* projection scale factors */
#ifndef lint
static char RCSID[] = "@(#)$Id: pj_factors.c,v 4.4 1992/07/14 01:28:57 gie Exp $";
#endif
#define _PJ_FACTORS
#define __PJ_LIB
#include "projects.h"

void
#ifdef __STDC__
pj_factors(LP lp, PJ *P)
#else
pj_factors(lp, P)
    LP lp;
    PJ *P;
#endif
{
	DERIVS *der;
	double cosphi, sinphi, t, r;

	if (!(der = pj_deriv(lp, pj_sfactors->fact_h, P))) {
		pj_sfactors->h = pj_sfactors->k = HUGE_VAL;
		return;
	}
	cosphi = cos(lp.phi);
	sinphi = sin(lp.phi);
	pj_sfactors->h = hypot(der->p_x_p, der->p_y_p);
	pj_sfactors->k = hypot(der->p_x_l, der->p_y_l) / cosphi;
	if (P->es) {
		pj_sfactors->h *= (r = (1. - P->es * sinphi * sinphi));
		pj_sfactors->h *= (t = sqrt(r)) / P->one_es;
		pj_sfactors->k *= t;
		r = P->one_es / (r * r);
	} else
		r = 1.;
	pj_sfactors->s = (der->p_y_p * der->p_x_l - der->p_x_p * der->p_y_l) /
		(r * cosphi);
	t = pj_sfactors->k * pj_sfactors->k + pj_sfactors->h * pj_sfactors->h;
	pj_sfactors->a = sqrt(t + 2. * pj_sfactors->s);
	t = (t = t - 2. * pj_sfactors->s) <= 0. ? 0. : sqrt(t);
	pj_sfactors->omega_2 = asin(t / pj_sfactors->a);
	pj_sfactors->b = 0.5 * (pj_sfactors->a + t);
	pj_sfactors->a = 0.5 * (pj_sfactors->a - t);
	return;
}
