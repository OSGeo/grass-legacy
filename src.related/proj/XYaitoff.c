#ifndef lint
static char *SCCSID = "@(#)XYaitoff.c	USGS v.3.3";
#endif
/*  Aitoff and Winkel Tripel Projections */
# include	"projects.h"
# define EPS	1e-8
	static
type;
	static double
cosphi1;
# undef INV_CODE
#ifdef FOR_CODE
FORWARD(s_forward); /* spheroid */
	double c, d;

	if (d = acos(cos(phi) * cos(0.5 * lam))) { /* basic Aitoff */
		c = sin(phi) / sin(d);
		if ((x = 1. - c * c) < EPS)
			if (x < -EPS) F_ERROR
			else x = 0.;
		else
			x = 2. * d * sqrt(x);
		if (lam < 0.) x = - x;
		y = d * c;
	} else
		x = y = 0.;
	if (type) { /* Winkel Tripel */
		x = (x + lam * cosphi1) * 0.5;
		y = (y + phi) * 0.5;
	}
	return (xy);
}
#else
NULL_FORWARD(s_forward);
#endif
#ifdef INV_CODE
INVERSE(s_inverse); /* spheroid */
	return (lp);
}
#else
NULL_INVERSE(s_inverse);
#endif
ENTRY(aitoff) {
	type = 0;
	if (inverse) RETURN(s_inverse); else RETURN(s_forward);
}
ENTRY(wintri) {
	type = 1;
	if ((cosphi1 = cos(*(*param)("rlat_1", "50.467"))) == 0.) {
		emess(1,"cos(lat_1) == 0.");
		E_ERROR;
	}
	if (inverse) RETURN(s_inverse); else RETURN(s_forward);
}
