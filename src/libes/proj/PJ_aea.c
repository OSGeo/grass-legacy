/* Albers Equal Area Conic */
#ifndef lint
static char RCSID[] = "@(#)$Id: PJ_aea.c,v 4.3 1992/07/14 01:25:49 gie Exp $";
#endif
# define EPS10	1.e-10
# define TOL7	1.e-7
#define __PROJ_PARMS \
	double	ec; \
	double	n; \
	double	c; \
	double	dd; \
	double	n2; \
	double	rho0; \
	double	rho; \
	double	phi1; \
	double	phi2; \
	double	*en; \
	int		ellips;
#define __PJ_LIB
# include	"projects.h"
/* determine latitude angle phi-1 */
# define N_ITER 15
# define EPSILON 1.0e-7
# define TOL 1.0e-10
	static double
#ifdef __STDC__
phi1_(double qs, double Te, double Tone_es)
#else
phi1_(qs, Te, Tone_es)
    double qs, Te, Tone_es;
#endif
{
	int i;
	double Phi, sinpi, cospi, con, com, dphi;

	Phi = asin (.5 * qs);
	if (Te < EPSILON)
		return( Phi );
	i = N_ITER;
	do {
		sinpi = sin (Phi);
		cospi = cos (Phi);
		con = Te * sinpi;
		com = 1. - con * con;
		dphi = .5 * com * com / cospi * (qs / Tone_es -
		   sinpi / com + .5 / Te * log ((1. - con) /
		   (1. + con)));
		Phi += dphi;
	} while (fabs(dphi) > TOL && --i);
	return( i ? Phi : HUGE_VAL );
}
FORWARD(e_forward) { XY xy;  /* ellipsoid & spheroid */
	if ((P->rho = P->c - (P->ellips ? P->n * pj_qsfn(sin(lp.phi),
		P->e, P->one_es) : P->n2 * sin(lp.phi))) < 0.) F_ERROR
	P->rho = P->dd * sqrt(P->rho);
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
		lp.phi =  P->rho / P->dd;
		if (P->ellips) {
			lp.phi = (P->c - lp.phi * lp.phi) / P->n;
			if (fabs(P->ec - fabs(lp.phi)) > TOL7) {
				if ((lp.phi = phi1_(lp.phi, P->e, P->one_es)) == HUGE_VAL)
					I_ERROR
			} else
				lp.phi = lp.phi < 0. ? -HALFPI : HALFPI;
		} else if (fabs(lp.phi = (P->c - lp.phi * lp.phi) / P->n2) <= 1.)
			lp.phi = asin(lp.phi);
		else
			lp.phi = lp.phi < 0. ? -HALFPI : HALFPI;
		lp.lam = atan2(xy.x, xy.y) / P->n;
	} else {
		lp.lam = 0.;
		lp.phi = P->n > 0. ? HALFPI : - HALFPI;
	}
	return (lp);
}
FREEUP {  if (P) { if (P->en) free(P->en); free(P); } }
	static PJ *

#ifdef __STDC__
setup(PJ *P)
#else
setup(P)
    PJ *P;
#endif
{
	double cosphi, sinphi;
	int secant;

	if (fabs(P->phi1 + P->phi2) < EPS10) {
		emess(-1,"lat_1 = -lat_2");
		E_ERROR;
	}
	P->n = sinphi = sin(P->phi1);
	cosphi = cos(P->phi1);
	secant = fabs(P->phi1 - P->phi2) >= EPS10;
	if (P->ellips = P->es > 0.) {
		double ml1, m1;

		if (!(P->en = pj_enfn(P->es)))
			E_ERROR;
		m1 = pj_msfn(sinphi, cosphi, P->es);
		ml1 = pj_qsfn(sinphi, P->e, P->one_es);
		if (secant) { /* secant cone */
			double ml2, m2;

			sinphi = sin(P->phi2);
			cosphi = cos(P->phi2);
			m2 = pj_msfn(sinphi, cosphi, P->es);
			ml2 = pj_qsfn(sinphi, P->e, P->one_es);
			P->n = (m1 * m1 - m2 * m2) / (ml2 - ml1);
		}
		P->ec = 1. - .5 * P->one_es * log((1. - P->e) /
			(1. + P->e)) / P->e;
		P->c = m1 * m1 + P->n * ml1;
		P->dd = 1. / P->n;
		P->rho0 = P->dd * sqrt(P->c - P->n * pj_qsfn(sin(P->phi0),
			P->e, P->one_es));
	} else {
		if (secant) P->n = .5 * (P->n + sin(P->phi2));
		P->n2 = P->n + P->n;
		P->c = cosphi * cosphi + P->n2 * sinphi;
		P->dd = 1. / P->n;
		P->rho0 = P->dd * sqrt(P->c - P->n2 * sin(P->phi0));
	}
	P->inv = e_inverse; P->fwd = e_forward;
	return P;
}



ENTRY(pj_aea) {
	if (!P)
		return((PJ *)malloc(sizeof(PJ)));
	P->phi1 = pj_param("rlat_1", "29.5")->f;
	P->phi2 = pj_param("rlat_2", "45.5")->f;
	return (setup(P));
}
ENTRY(pj_leac) {
	if (!P)
		return((PJ *)malloc(sizeof(PJ)));
	P->phi2 = pj_param("rlat_1", "")->f;
	P->phi1 = pj_param("bsouth", "F")->i ? - HALFPI: HALFPI;
	return (setup(P));
}
