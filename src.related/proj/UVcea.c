#ifndef lint
static char *SCCSID = "@(#)UVcea.c	USGS v.3.1";
#endif
/* Equal Area Cylindrical projection */
# include	"projects.h"
	static double
k0, qp;
# define EPS	1e-10
#ifdef FOR_CODE
FORWARD(e_forward); /* spheroid */
	x = k0 * lam;
	y = .5 * qsfn_(sin(phi)) / k0;
	return (xy);
}
FORWARD(s_forward); /* spheroid */
	x = k0 * lam;
	y = sin(phi) / k0;
	return (xy);
}
#else
NULL_FORWARD(s_forward);
NULL_FORWARD(e_forward);
#endif
#ifdef INV_CODE
INVERSE(e_inverse); /* spheroid */
	phi = authlat(asin( 2. * y * k0 / qp));
	lam = x / k0;
	return (lp);
}
INVERSE(s_inverse); /* spheroid */
	double t;

	if ((t = fabs(y *= k0)) - EPS <= 1.) {
		if (t >= 1.)
			phi = y < 0. ? -HALFPI : HALFPI;
		else
			phi = asin(y);
		lam = x / k0;
	} else I_ERROR;
	return (lp);
}
#else
NULL_INVERSE(s_inverse);
NULL_INVERSE(e_inverse);
#endif
ENTRY(cea) {
	double t;

	if ((k0 = cos(t = *(*param)("rlat_ts", ""))) < 0.) {
		emess(-1,"lat_ts >= 90");
		E_ERROR;
	}
	if (es) {
		t = sin(t);
		k0 /= sqrt(1. - es * t * t);
		e = sqrt(es);
		authset();
		qp = qsfn_(1.);
		if (inverse) RETURN(e_inverse); else RETURN(e_forward);
	} else	if (inverse) RETURN(s_inverse); else RETURN(s_forward);
}
