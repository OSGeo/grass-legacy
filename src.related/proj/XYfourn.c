#ifndef lint
static char *SCCSID = "@(#)XYfourn.c	USGS v.3.2";
#endif
/*  Fournier Globular I */
# include	"projects.h"
# define EPS	1e-10
# define C	2.46740110027233965467
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
	} else {
		double p, s, at;

		p = fabs(PI * sin(phi));
		s = (C - phi * phi)/(p - 2. * fabs(phi));
		at = lam * lam / C - 1.;
		if ((y = s * s - at * (C - p * s - lam * lam)) < 0.) {
			if (y < -EPS) F_ERROR
			else y = -s / at;
		} else
			y = (sqrt(y) - s) / at;
		if (phi < 0.) y = -y;
		if ((x = 1. - y * y / C) < 0.) {
			if (x < -EPS) F_ERROR
			else x = 0;
		} else
			x = lam * sqrt(x);
	}
	return (xy);
}
#else
NULL_FORWARD(s_forward);
#endif
NULL_INVERSE(s_inverse);
ENTRY(fourn) { if (inverse) RETURN(s_inverse); else RETURN(s_forward); }
