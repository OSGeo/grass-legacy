static char *SCCSID = "@(#)aea.c	AMG v.1.1";
/* Albers Equal Area Conic */
# define	PROJ_MOD
# include	"projects.h"
struct _PROJ { PHEAD;
	double	PHI0, PHI1, PHI2, E, EC, N, RHO0, C, N2, D;
	int	ELLIPS;
};
# define EPS10	1.e-10
# define TOL7	1.e-7
# define ec	proj->EC
# define n	proj->N
# define c	proj->C
# define d	proj->D
# define n2	proj->N2
# define rho0	proj->RHO0
# define ellips proj->ELLIPS
	static double
rho;
FORWARD(forward); /* ellipsoid & spheroid */
	if ((rho = c - (ellips ? n * qsfn_(e, sin(phi), cos(phi)) :
	   n2 * sin(phi))) < 0.) ERROR;
	rho = d * sqrt(rho);
	x = rho * sin( lam *= n );
	y = rho0 - rho * cos(lam);
	return (&xy);
}
INVERSE(inverse) /* ellipsoid & spheroid */;
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
				if ((phi = phi1_(e, phi)) == HUGE)
					ERROR;
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
	return (&lp);
}
ENTRY(aea) {
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
		ml1 = qsfn_(e, sinphi, cosphi);
		if (secant) { /* secant cone */
			double ml2, m2;

			sinphi = sin(phi2);
			cosphi = cos(phi2);
			m2 = msfn_(e, sinphi, cosphi);
			ml2 = qsfn_(e, sinphi, cosphi);
			n = (m1 * m1 - m2 * m2) / (ml2 - ml1);
		}
		ec = 1. - .5 * (1. - es) * log((1. - e) / (1. + e)) / e;
		c = m1 * m1 + n * ml1;
		d = 1. / n;
		rho0 = d * sqrt(c - n * qsfn_(e, sin(phi0), cos(phi0)));
	} else {
		if (secant) n = .5 * (n + sin(phi2));
		n2 = n + n;
		c = cosphi * cosphi + n2 * sinphi;
		d = 1. / n;
		rho0 = d * sqrt(c - n2 * sin(phi0));
	}
	return (1);
}
