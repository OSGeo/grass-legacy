#ifndef lint
static char *SCCSID = "@(#)UVeqdc.c	USGS v.3.1";
#endif
/* Equidistant Conic */
# include	"projects.h"
	static double
phi1, phi2, n, rho, rho0, c;
	static int
ellips;
# define EPS10	1.e-10
#ifdef FOR_CODE
FORWARD(e_forward); /* sphere & ellipsoid */
	rho = c - (ellips ? mlfn_(phi, sin(phi), cos(phi)) : phi);
	x = rho * sin( lam *= n );
	y = rho0 - rho * cos(lam);
	return (xy);
}
#else
NULL_FORWARD(e_forward);
#endif
#ifdef INV_CODE
INVERSE(e_inverse); /* sphere & ellipsoid */
	if (rho = hypot(x, y = rho0 - y)) {
		if (n < 0.) {
			rho = -rho;
			x = -x;
			y = -y;
		}
		phi = c - rho;
		if (ellips)
			phi = inv_mlfn_(phi);
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
ENTRY(eqdc) {
	double cosphi, sinphi;
	int secant;

	phi1 = *(*param)("rlat_1", "29.5");
	phi2 = *(*param)("rlat_2", "45.5");
	if (fabs(phi1 + phi2) < EPS10) {
		emess(-1,"lat_1 = - lat_2");
		E_ERROR;
	}
	n = sinphi = sin(phi1);
	cosphi = cos(phi1);
	secant = fabs(phi1 - phi2) >= EPS10;
	if (ellips = es > 0.) {
		double ml1, m1;

		m1 = msfn_(sinphi, cosphi);
		enfn_();
		ml1 = mlfn_(phi1, sinphi, cosphi);
		if (secant) { /* secant cone */
			sinphi = sin(phi2);
			cosphi = cos(phi2);
			n = (m1 - msfn_(sinphi, cosphi)) /
				(mlfn_(phi2, sinphi, cosphi) - ml1);
		}
		c = ml1 + m1 / n;
		rho0 = c - mlfn_(phi0, sin(phi0), cos(phi0));
	} else {
		if (secant)
			n = (cosphi - cos(phi2)) / (phi2 - phi1);
		c = phi1 + cos(phi1) / n;
		rho0 = c - phi0;
	}
	if (inverse) RETURN(e_inverse); else RETURN(e_forward);
}
