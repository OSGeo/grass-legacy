static char *SCCSID = "@(#)eqdc.c	AMG v.1.1";
/* Equidistant Conic */
# define	PROJ_MOD
# include	"projects.h"
struct _PROJ { PHEAD;
	double PHI0, PHI1, PHI2;
	double E, E0[E_MAX], N, RHO0, C;
	int ELLIPS;
};
# define EPS10	1.e-10
# define n	proj->N
# define c	proj->C
# define rho0	proj->RHO0
# define ellips proj->ELLIPS
	static double
rho;
FORWARD(forward); /* spheroid & ellipsoid */
	rho = c - (ellips ? mlfn_(e0, phi) : phi);
	x = rho * sin( lam *= n );
	y = rho0 - rho * cos(lam);
	return (&xy);
}
INVERSE(inverse); /* spheroid & ellipsoid */
	if (rho = hypot(x, y = rho0 - y)) {
		if (n < 0.) {
			rho = -rho;
			x = -x;
			y = -y;
		}
		phi = c - rho;
		if (ellips && (phi = phi3_(rho, e0)) == HUGE) ERROR;
		lam = atan2(x, y) / n;
	} else {
		lam = 0.;
		phi = n > 0. ? HALFPI : - HALFPI;
	}
	return (&lp);
}
ENTRY(eqdc) {
	double cosphi, sinphi;
	int secant;

	if (! proj) return (sizeof(struct _PROJ));
	if (comset(I_ALL, proj, param)) ERROR;
	phi0 = *(*param)("rlat_0", "");
	phi1 = *(*param)("rlat_1", "29.5");
	phi2 = *(*param)("rlat_2", "45.5");
	if (fabs(phi1 + phi2) < EPS10) ERROR;
	proj->forward = forward;
	proj->inverse = inverse;
	n = sinphi = sin(phi1);
	cosphi = cos(phi1);
	secant = fabs(phi1 - phi2) >= EPS10;
	if (ellips = es > 0.) {
		double ml1, m1;

		m1 = msfn_(e = sqrt(es), sinphi, cosphi);
		enfn_(e0, es);
		ml1 = mlfn_(e0, phi1);
		if (secant) { /* secant cone */
			sinphi = sin(phi2);
			cosphi = cos(phi2);
			n = (m1 - msfn_(e, sinphi, cosphi)) /
			   (mlfn_(e0, phi2) - ml1);
		}
		c = ml1 + m1 / n;
		rho0 = c - mlfn_(e0, phi0);
	} else {
		if (secant)
			n = (cosphi - cos(phi2)) / (phi2 - phi1);
		c = phi1 + cos(phi1) / n;
		rho0 = c - phi0;
	}
	return (1);
}
