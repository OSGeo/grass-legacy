#ifndef lint
static char *SCCSID = "@(#)UVcass.c	USGS v.3.1";
#endif
/* Cassini projection */
# include	"projects.h"
# define EPS10	1e-10
# define C1	.16666666666666666666
# define C2	.00833333333333333333
# define C3	.04166666666666666666
# define C4	.33333333333333333333
# define C5	.06666666666666666666
	static double
m0, n, t, a1, c, r, d, d2, a2, tn;
#ifdef FOR_CODE
FORWARD(e_forward); /* ellipsoid */
	y = mlfn_(phi, n = sin(phi), c = cos(phi));
	n = 1./sqrt(1. - es * n * n);
	tn = tan(phi); t = tn * tn;
	a1 = lam * c;
	c *= es * c / (1 - es);
	a2 = a1 * a1;
	x = n * a1 * (1. - a2 * t * (C1 - (8. - t + 8. * c) * a2 * C2));
	y -= m0 - n * tn * a2 * (.5 + (5. - t + 6. * c) * a2 * C3);
	return (xy);
}
FORWARD(s_forward); /* spheroid */
	x = asin(cos(phi) * sin(lam));
	y = atan2(tan(phi) , cos(lam)) - phi0;
	return (xy);
}
#else
NULL_FORWARD(s_forward);
NULL_FORWARD(e_forward);
#endif
#ifdef INV_CODE
INVERSE(e_inverse); /* ellipsoid */
	double ph1;

	ph1 = inv_mlfn_(m0 + y);
	tn = tan(ph1); t = tn * tn;
	n = sin(ph1);
	r = 1. / (1. - es * n * n);
	n = sqrt(r);
	r *= (1. - es) * n;
	d = x / n;
	d2 = d * d;
	phi = ph1 - (n * tn / r) * d2 * (.5 - (1. + 3. * t) * d2 * C3);
	lam = d * (1. + t * d2 * (-C4 + (1. + 3. * t) * d2 * C5)) / cos(ph1);
	return (lp);
}
INVERSE(s_inverse); /* spheroid */
	phi = asin(sin(d = y + phi0) * cos(x));
	lam = atan2(tan(x), cos(d));
	return (lp);
}
#else
NULL_INVERSE(s_inverse);
NULL_INVERSE(e_inverse);
#endif
ENTRY(cass) {
	if (es) {
		enfn_();
		m0 = mlfn_(phi0, sin(phi0), cos(phi0));
		if (inverse) RETURN(e_inverse); else RETURN(e_forward);
	} else	if (inverse) RETURN(s_inverse); else RETURN(s_forward);
}
