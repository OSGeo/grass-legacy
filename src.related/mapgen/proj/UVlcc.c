#ifndef lint
static char *SCCSID = "@(#)UVlcc.c	USGS v.3.1";
#endif
/* Lambert Conformal Conic */
# include	"projects.h"
	static double
phi1, phi2, n, rho, rho0, c;
	static int
ellips;
# define EPS10	1.e-10
#ifdef FOR_CODE
FORWARD(e_forward); /* ellipsoid & spheroid */
	if (fabs(fabs(phi) - HALFPI) < EPS10) {
		if ((phi * n) <= 0.) F_ERROR;
		rho = 0.;
		}
	else
		rho = c * (ellips ? pow(tsfn_(phi, sin(phi)), n) :
		   pow(tan(FORTPI + .5 * phi), -n));
	x = rho * sin( lam *= n );
	y = rho0 - rho * cos(lam);
	return (xy);
}
#else
NULL_FORWARD(e_forward);
#endif
#ifdef INV_CODE
INVERSE(e_inverse); /* ellipsoid & spheroid */
	if (rho = hypot(x, y = rho0 - y)) {
		if (n < 0.) {
			rho = -rho;
			x = -x;
			y = -y;
		}
		if (ellips) {
			if ((phi = phi2_(pow(rho / c, 1./n))) == HUGE)
				I_ERROR;
		} else
			phi = 2. * atan(pow(c / rho, 1./n)) - HALFPI;
		lam = atan2(x, y) / n;
	} else {
		lam = 0.;
		phi = n > 0. ? HALFPI : - HALFPI;
	}
	return (lp);
}
#else
NULL_INVERSE(e_inverse);
#endif
ENTRY(lcc) {
	double cosphi, sinphi;
	int secant;

	phi1 = *(*param)("rlat_1", "33.");
	phi2 = *(*param)("rlat_2", "45.");
	if (fabs(phi1 + phi2) < EPS10) {
		emess(-1,"lat_1 = - lat_2");
		E_ERROR;
	}
	n = sinphi = sin(phi1);
	cosphi = cos(phi1);
	secant = fabs(phi1 - phi2) >= EPS10;
	if (ellips = es != 0.) {
		double ml1, m1;

		e = sqrt(es);
		m1 = msfn_(sinphi, cosphi);
		ml1 = tsfn_(phi1, sinphi);
		if (secant) { /* secant cone */
			n = log(m1 /
			   msfn_(sinphi = sin(phi2), cos(phi2)));
			n /= log(ml1 / tsfn_(phi2, sinphi));
		}
		c = (rho0 = m1 * pow(ml1, -n) / n);
		rho0 *= pow(tsfn_(phi0, sin(phi0)), n);
	} else {
		if (secant)
			n = log(cosphi / cos(phi2)) /
			   log(tan(FORTPI + .5 * phi2) /
			   tan(FORTPI + .5 * phi1));
		c = cosphi * pow(tan(FORTPI + .5 * phi1), n) / n;
		rho0 = c * pow(tan(FORTPI + .5 * phi0), -n);
	}
	if (inverse) RETURN(e_inverse); else RETURN(e_forward);
}
