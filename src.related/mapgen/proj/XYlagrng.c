#ifndef lint
static char *SCCSID = "@(#)XYlagrng.c	USGS v.3.4";
#endif
/*  Lagrange Projection */
# include	"projects.h"
# define TOL	1e-10
	static double
hrw, rw, a1;
#ifdef FOR_CODE
FORWARD(s_forward); /* spheroid */
	double al, v, c;

	if (fabs(fabs(phi) - HALFPI) < TOL) {
		x = 0;
		y = phi < 0 ? -2. : 2.;
	} else {
		phi = sin(phi);
		v = a1 * pow((1. + phi)/(1. - phi), hrw);
		if ((c = 0.5 * (v + 1./v) + cos(lam *= rw)) < TOL)
			F_ERROR;
		x = 2. * sin(lam) / c;
		y = (v - 1./v) / c;
	}
	return (xy);
}
#else
NULL_FORWARD(s_forward);
#endif
NULL_FORWARD(s_inverse);
ENTRY(lagrng) {
	double phi1;

	if ((rw = *(*param)("dW", "2")) <= 0)
		emess(1,"W <= 0");
	hrw = 0.5 * (rw = 1. / rw);
	phi1 = *(*param)("rlat_1", "0");
	if (fabs(fabs(phi1 = sin(phi1)) - 1.) < TOL)
		emess(1,"|phi_1| == 90");
	a1 = pow((1. - phi1)/(1. + phi1), hrw);
	if (inverse) RETURN(s_inverse); else RETURN(s_forward);
}
