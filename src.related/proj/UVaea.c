#ifndef lint
static char *SCCSID = "@(#)UVaea.c	USGS v.3.2";
#endif
/* Albers Equal Area Conic */
# include	"projects.h"
# define EPS10	1.e-10
# define TOL7	1.e-7
	static int
ellips;
	static double
ec, n, c, d, n2, rho0, rho, phi1, phi2;
#ifdef FOR_CODE
FORWARD(e_forward); /* ellipsoid & spheroid */
	if ((rho = c - (ellips ? n * qsfn_(sin(phi)) :
	   n2 * sin(phi))) < 0.) F_ERROR;
	rho = d * sqrt(rho);
	x = rho * sin( lam *= n );
	y = rho0 - rho * cos(lam);
	return (xy);
}
#else
NULL_FORWARD(e_forward);
#endif
#ifdef INV_CODE
INVERSE(e_inverse) /* ellipsoid & spheroid */;
	if (rho = hypot(x, y = rho0 - y)) {
		if (n < 0.) {
			rho = -rho;
			x = -x;
			y = -y;
		}
		phi =  rho / d;
		if (ellips) {
			phi = (c - phi * phi) / n;
			if (fabs(ec - fabs(phi)) > TOL7) {
				if ((phi = phi1_(phi)) == HUGE)
					I_ERROR;
			} else
				phi = phi < 0. ? -HALFPI : HALFPI;
		} else if (fabs(phi = (c - phi * phi) / n2) <= 1.)
			phi = asin(phi);
		else
			phi = phi < 0. ? -HALFPI : HALFPI;
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
	static
ENTRY(setup) {
	double cosphi, sinphi;
	int secant;

	if (fabs(phi1 + phi2) < EPS10) {
		emess(-1,"lat_1 = -lat_2");
		E_ERROR;
	}
	n = sinphi = sin(phi1);
	cosphi = cos(phi1);
	secant = fabs(phi1 - phi2) >= EPS10;
	if (ellips = es > 0.) {
		double ml1, m1;

		m1 = msfn_(sinphi, cosphi);
		ml1 = qsfn_(sinphi);
		if (secant) { /* secant cone */
			double ml2, m2;

			sinphi = sin(phi2);
			cosphi = cos(phi2);
			m2 = msfn_(sinphi, cosphi);
			ml2 = qsfn_(sinphi);
			n = (m1 * m1 - m2 * m2) / (ml2 - ml1);
		}
		ec = 1. - .5 * one_es * log((1. - e) / (1. + e)) / e;
		c = m1 * m1 + n * ml1;
		d = 1. / n;
		rho0 = d * sqrt(c - n * qsfn_(sin(phi0)));
	} else {
		if (secant) n = .5 * (n + sin(phi2));
		n2 = n + n;
		c = cosphi * cosphi + n2 * sinphi;
		d = 1. / n;
		rho0 = d * sqrt(c - n2 * sin(phi0));
	}
	if (inverse) RETURN(e_inverse); else RETURN(e_forward);
}
ENTRY(aea) {
	phi1 = *(*param)("rlat_1", "29.5");
	phi2 = *(*param)("rlat_2", "45.5");
	return (setup(inverse));
}
ENTRY(leac) {
	phi2 = *(*param)("rlat_1", "0");
	phi1 = *(int *)(*param)("bsouth", "") ? - HALFPI: HALFPI;
	return (setup(inverse));
}
