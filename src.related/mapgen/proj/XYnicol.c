#ifndef lint
static char *SCCSID = "@(#)XYnicol.c	USGS v.3.2";
#endif
/*  Nicolosi Globular */
# include	"projects.h"
# define EPS	1e-10
#ifdef FOR_CODE
FORWARD(s_forward); /* spheroid */
	if (fabs(lam) < EPS) {
		x = 0;
		y = phi;
	} else if (fabs(phi) < EPS) {
		x = lam;
		y = 0.;
	} else if (fabs(fabs(lam) - HALFPI) < EPS) {
		x = lam * cos(phi);
		y = HALFPI * sin(phi);
	} else if (fabs(fabs(phi) - HALFPI) < EPS) {
		x = 0;
		y = phi;
	} else {
		double tb, c, d, m, n, r2, sp;

		tb = HALFPI / lam - lam / HALFPI;
		c = phi / HALFPI;
		d = (1 - c * c)/((sp = sin(phi)) - c);
		r2 = tb / d;
		r2 *= r2;
		m = (tb * sp / d - 0.5 * tb)/(1. + r2);
		n = (sp / r2 + 0.5 * d)/(1. + 1./r2);
		x = cos(phi);
		x = sqrt(m * m + x * x / (1. + r2));
		x = HALFPI * ( m + (lam < 0. ? -x : x));
		y = sqrt(n * n - (sp * sp / r2 + d * sp - 1.) /
			(1. + 1./r2));
		y = HALFPI * ( n + (phi < 0. ? y : -y ));
	}
	return (xy);
}
#else
NULL_FORWARD(s_forward);
#endif
NULL_INVERSE(s_inverse);
ENTRY(nicol) { if (inverse) RETURN(s_inverse); else RETURN(s_forward); }
