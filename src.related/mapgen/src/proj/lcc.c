static char *SCCSID = "@(#)lcc.c	AMG v.1.1";
/* Lambert Conformal Conic */
# define	PROJ_MOD
# include	"projects.h"
struct _PROJ { PHEAD;
	double PHI0, PHI1, PHI2;
	double E, N, RHO0, C;
	int ELLIPS;
};
# define EPS10	1.e-10
# define n	proj->N
# define c	proj->C
# define d	proj->D
# define rho0	proj->RHO0
# define ellips	proj->ELLIPS
	static double
rho;
FORWARD(forward); /* ellipsoid & spheroid */
	if (fabs(fabs(phi) - HALFPI) < EPS10) {
		if ((phi * n) <= 0.) ERROR;
		rho = 0.;
		}
	else
		rho = c * (ellips ? pow(tsfn_(e, phi, sin(phi)), n) :
		   pow(tan(FORTPI + .5 * phi), -n));
	x = rho * sin( lam *= n );
	y = rho0 - rho * cos(lam);
	return (&xy);
}
INVERSE(inverse); /* ellipsoid & spheroid */
	if (rho = hypot(x, y = rho0 - y)) {
		if (n < 0.) {
			rho = -rho;
			x = -x;
			y = -y;
		}
		if (ellips) {
			if ((phi = phi2_(e, pow(rho / c, 1./n))) == HUGE)
				ERROR;
		} else
			phi = 2. * atan(pow(c / rho, 1./n)) - HALFPI;
		lam = atan2(x, y) / n;
	} else {
		lam = 0.;
		phi = n > 0. ? HALFPI : - HALFPI;
	}
	return (&lp);
}
ENTRY(lcc) {
	double cosphi, sinphi;
	int secant;

	if (! proj) return (sizeof(struct _PROJ));
	if (comset(I_ALL, proj, param)) ERROR;
	phi0 = *(*param)("rlat_0", "");
	phi1 = *(*param)("rlat_1", "33.");
	phi2 = *(*param)("rlat_2", "45.");
	if (fabs(phi1 + phi2) < EPS10) ERROR;
	proj->forward = forward;
	proj->inverse = inverse;
	n = sinphi = sin(phi1);
	cosphi = cos(phi1);
	secant = fabs(phi1 - phi2) >= EPS10;
	if (ellips = es != 0.) {
		double ml1, m1;

		e = sqrt(es);
		m1 = msfn_(e, sinphi, cosphi);
		ml1 = tsfn_(e, phi1, sinphi);
		if (secant) { /* secant cone */
			n = log(m1 /
			   msfn_(e, sinphi = sin(phi2), cos(phi2)));
			n /= log(ml1 / tsfn_(e, phi2, sinphi));
		}
		c = (rho0 = m1 * pow(ml1, -n) / n);
		rho0 *= pow(tsfn_(e, phi0, sin(phi0)), n);
	} else {
		if (secant)
			n = log(cosphi / cos(phi2)) /
			   log(tan(FORTPI + .5 * phi2) /
			   tan(FORTPI + .5 * phi1));
		c = cosphi * pow(tan(FORTPI + .5 * phi1), n) / n;
		rho0 = c * pow(tan(FORTPI + .5 * phi0), -n);
	}
	return (1);
}
