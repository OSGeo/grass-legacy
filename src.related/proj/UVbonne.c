#ifndef lint
static char *SCCSID = "@(#)UVbonne.c	USGS v.3.2";
#endif
/* Bonne projection */
# include	"projects.h"
	static double
phi1, cphi1, am1, m1;
# define EPS10	1e-10
#ifdef FOR_CODE
FORWARD(e_forward); /* ellipsoid */
	double rh, E, c;

	rh = am1 + m1 - mlfn_(phi, E = sin(phi), c = cos(phi));
	E = c * lam / (rh * sqrt(1. - es * E * E));
	x = rh * sin(E);
	y = am1 - rh * cos(E);
	return (xy);
}
FORWARD(s_forward); /* spheroid */
	double E, rh;

	rh = cphi1 + phi1 - phi;
	if (fabs(rh) > EPS10) {
		x = rh * sin(E = lam * cos(phi) / rh);
		y = cphi1 - rh * cos(E);
	} else
		x = y = 0.;
	return (xy);
}
#else
NULL_FORWARD(s_forward);
NULL_FORWARD(e_forward);
#endif
#ifdef INV_CODE
INVERSE(s_inverse); /* spheroid */
	double rh;

	rh = hypot(x, y = cphi1 - y);
	phi = cphi1 + phi1 - rh;
	if (fabs(phi) > HALFPI) I_ERROR;
	if (fabs(fabs(phi) - HALFPI) <= EPS10)
		lam = 0.;
	else
		lam = rh * atan2(x, y) / cos(phi);
	return (lp);
}
INVERSE(e_inverse); /* ellipsoid */
	double s, rh;

	rh = hypot(x, y = am1 - y);
	phi = inv_mlfn_(am1 + m1 - rh);
	if ((s = fabs(phi)) < HALFPI) {
		s = sin(phi);
		lam = rh * atan2(x, y) *
		   sqrt(1. - es * s * s) / cos(phi);
	} else if (fabs(s - HALFPI) <= EPS10)
		lam = 0.;
	else I_ERROR;
	return (lp);
}
#else
NULL_INVERSE(s_inverse);
NULL_INVERSE(e_inverse);
#endif
ENTRY(bonne) {
	double c;

	phi1 = *(*param)("rlat_1", "40.");
	if (fabs(phi1) < EPS10) {
		emess(-1,"|phi1| < %.2e", EPS10);
		E_ERROR;
	}
	if (es) {
		enfn_();
		m1 = mlfn_(phi1, am1 = sin(phi1), c = cos(phi1));
		am1 = c / (sqrt(1. - es * am1 * am1) * am1);
		if (inverse) RETURN(e_inverse); else RETURN(e_forward);
	} else {
		if (fabs(phi1) + EPS10 >= HALFPI)
			cphi1 = 0.;
		else
			cphi1 = 1. / tan(phi1);
		if (inverse) RETURN(s_inverse); else RETURN(s_forward);
	}
}
