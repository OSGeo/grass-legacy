#ifndef lint
static char *SCCSID = "@(#)XYrpoly.c	USGS v.3.2";
#endif
/*  Rectangular Polyconic */
# include	"projects.h"
# define EPS	1e-9
	static int
mode;
	static double
phi1, fxa, fxb;
#ifdef FOR_CODE
FORWARD(s_forward); /* spheroid */
	double fa;

	if (mode)
		fa = tan(lam * fxb) * fxa;
	else
		fa = 0.5 * lam;
	if (fabs(phi) < EPS) {
		x = fa + fa;
		y = - phi0;
	} else {
		y = 1. / tan(phi);
		x = sin(fa = 2. * atan(fa * sin(phi))) * y;
		y = phi - phi0 + (1. - cos(fa)) * y;
	}
	return (xy);
}
#else
NULL_FORWARD(s_forward);
#endif
NULL_INVERSE(s_inverse);
ENTRY(rpoly) {
	if ((mode = (phi1 = fabs(*(*param)("rlat_ts","0"))) > EPS)) {
		fxb = 0.5 * sin(phi1);
		fxa = 0.5 / fxb;
	}
	if (inverse) RETURN(s_inverse); else RETURN(s_forward);
}
