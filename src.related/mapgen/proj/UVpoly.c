#ifndef lint
static char *SCCSID = "@(#)UVpoly.c	USGS v.3.1";
#endif
/* Polyconic (American) projection */
# include "projects.h"
	static double
ml0;
# define TOL	1e-10
# define CONV	1e-10
# define N_ITER	10
#ifdef FOR_CODE
FORWARD(e_forward); /* ellipsoid */
	double  ms, sp, cp;

	if (fabs(phi) <= TOL) { x = lam; y = -ml0; }
	else {
		sp = sin(phi);
		ms = fabs(cp = cos(phi)) > TOL ? msfn_(sp, cp) / sp : 0.;
		x = ms * sin(lam *= sp);
		y = (mlfn_(phi, sp, cp) - ml0) + ms * (1. - cos(lam));
	}
	return (xy);
}
FORWARD(s_forward); /* spheroid */
	double  cot, E;

	if (fabs(phi) <= TOL) { x = lam; y = ml0; }
	else {
		cot = 1. / tan(phi);
		x = sin(E = lam * sin(phi)) * cot;
		y = phi - phi0 + cot * (1. - cos(E));
	}
	return (xy);
}
#else
NULL_FORWARD(s_forward);
NULL_FORWARD(e_forward);
#endif
#ifdef INV_CODE
INVERSE(e_inverse); /* ellipsoid */
	double c;

	y += ml0;
	if (fabs(y) <= TOL) { lam = x; phi = 0.; }
	else {
		if((phi = phi4_(y, y * y + x * x)) == HUGE)
			I_ERROR;
		c = sin(phi);
		lam = asin(x * tan(phi) * sqrt(1. - es * c * c)) / sin(phi);
	}
	return (lp);
}
INVERSE(s_inverse); /* spheroid */
	double B, dphi, tp;
	int i;

	if (fabs(y = phi0 + y) <= TOL) { lam = x; phi = 0.; }
	else {
		phi = y;
		B = x * x + y * y;
		i = N_ITER;
		do {
			tp = tan(phi);
			phi -= (dphi = (y * (phi * tp + 1.) - phi -
				.5 * ( phi * phi + B) * tp) /
				((phi - y) / tp - 1.));
		} while (fabs(dphi) > CONV && --i);
		if (! i) I_ERROR;
		lam = asin(x * tan(phi)) / sin(phi);
	}
	return (lp);
}
#else
NULL_INVERSE(s_inverse);
NULL_INVERSE(e_inverse);
#endif
ENTRY(poly) {
	if (es) {
		enfn_();
		ml0 = mlfn_(phi0, sin(phi0), cos(phi0));
		if (inverse) RETURN(e_inverse); else RETURN(e_forward);
	} else {
		ml0 = -phi0;
		if (inverse) RETURN(s_inverse); else RETURN(s_forward);
	}
}
