/* Lambert Conformal Conic */
#ifndef lint
static char RCSID[] = "@(#)$Id: PJ_lcc.c,v 4.2 1992/07/14 01:27:33 gie Exp $";
#endif
#define __PROJ_PARMS \
	double	phi1; \
	double	phi2; \
	double	n; \
	double	rho; \
	double	rho0; \
	double	c; \
	int		ellips;
#define __PJ_LIB
#include	"projects.h"
# define EPS10	1.e-10
FORWARD(e_forward) { XY xy;  /* ellipsoid & spheroid */
	if (fabs(fabs(lp.phi) - HALFPI) < EPS10) {
		if ((lp.phi * P->n) <= 0.) F_ERROR;
		P->rho = 0.;
		}
	else
		P->rho = P->c * (P->ellips ? pow(pj_tsfn(lp.phi, sin(lp.phi),
			P->e), P->n) : pow(tan(FORTPI + .5 * lp.phi), -P->n));
	xy.x = P->rho * sin( lp.lam *= P->n );
	xy.y = P->rho0 - P->rho * cos(lp.lam);
	return (xy);
}
INVERSE(e_inverse) { LP lp;  /* ellipsoid & spheroid */
	if (P->rho = hypot(xy.x, xy.y = P->rho0 - xy.y)) {
		if (P->n < 0.) {
			P->rho = -P->rho;
			xy.x = -xy.x;
			xy.y = -xy.y;
		}
		if (P->ellips) {
			if ((lp.phi = pj_phi2(pow(P->rho / P->c, 1./P->n), P->e))
				== HUGE_VAL)
				I_ERROR;
		} else
			lp.phi = 2. * atan(pow(P->c / P->rho, 1./P->n)) - HALFPI;
		lp.lam = atan2(xy.x, xy.y) / P->n;
	} else {
		lp.lam = 0.;
		lp.phi = P->n > 0. ? HALFPI : - HALFPI;
	}
	return (lp);
}
FREEUP {  if (P) free(P); }
ENTRY(pj_lcc) {
	double cosphi, sinphi;
	int secant;

	if (!P)
		return((PJ *)malloc(sizeof(PJ)));
	P->phi1 = pj_param("rlat_1", "33.")->f;
	P->phi2 = pj_param("rlat_2", "45.")->f;
	if (fabs(P->phi1 + P->phi2) < EPS10) {
		emess(-1,"lat_1 = - lat_2");
		E_ERROR;
	}
	P->n = sinphi = sin(P->phi1);
	cosphi = cos(P->phi1);
	secant = fabs(P->phi1 - P->phi2) >= EPS10;
	if (P->ellips = (P->es != 0.)) {
		double ml1, m1;

		P->e = sqrt(P->es);
		m1 = pj_msfn(sinphi, cosphi, P->es);
		ml1 = pj_tsfn(P->phi1, sinphi, P->e);
		if (secant) { /* secant cone */
			P->n = log(m1 /
			   pj_msfn(sinphi = sin(P->phi2), cos(P->phi2), P->es));
			P->n /= log(ml1 / pj_tsfn(P->phi2, sinphi, P->e));
		}
		P->c = (P->rho0 = m1 * pow(ml1, -P->n) / P->n);
		P->rho0 *= pow(pj_tsfn(P->phi0, sin(P->phi0), P->e), P->n);
	} else {
		if (secant)
			P->n = log(cosphi / cos(P->phi2)) /
			   log(tan(FORTPI + .5 * P->phi2) /
			   tan(FORTPI + .5 * P->phi1));
		P->c = cosphi * pow(tan(FORTPI + .5 * P->phi1), P->n) / P->n;
		P->rho0 = P->c * pow(tan(FORTPI + .5 * P->phi0), -P->n);
	}
	P->inv = e_inverse;
	P->fwd = e_forward;
	P->pfree = freeup;
	return P;
}
