#ifndef lint
static char *SCCSID = "@(#)UVmerc.c	USGS v.3.1";
#endif
/* Mercator projection */
# include	"projects.h"
	static double
ap;
# define EPS10 1.e-10
#ifdef FOR_CODE
FORWARD(e_forward); /* ellipsoid */
	if (fabs(fabs(phi) - HALFPI) <= EPS10) F_ERROR;
	x = ap * lam;
	y = - ap * log(tsfn_(phi, sin(phi)));
	return (xy);
}
FORWARD(s_forward); /* spheroid */
	if (fabs(fabs(phi) - HALFPI) <= EPS10) F_ERROR;
	x = ap * lam;
	y = ap * log(tan(FORTPI + .5 * phi));
	return (xy);
}
#else
NULL_FORWARD(s_forward);
NULL_FORWARD(e_forward);
#endif
#ifdef INV_CODE
INVERSE(e_inverse); /* ellipsoid */
	if ((phi = phi2_(exp(- y / ap))) == HUGE) I_ERROR;
	lam = x / ap;
	return (lp);
}
INVERSE(s_inverse); /* spheroid */
	phi = HALFPI - 2. * atan(exp(-y / ap));
	lam = x / ap;
	return (lp);
}
#else
NULL_INVERSE(s_inverse);
NULL_INVERSE(e_inverse);
#endif
ENTRY(merc) {
	double phits;

	phits = fabs(*(*param)("rlat_ts", ""));
	if (phits >= HALFPI) {
		emess(-1,"lat_ts >= 90d");
		E_ERROR;
	}
	if (es) { /* ellipsoid */
		ap = msfn_(sin(phits), cos(phits));
		if (inverse) RETURN(e_inverse); else RETURN(e_forward);
	} else { /* sphere */
		ap = cos(phits);
		if (inverse) RETURN(s_inverse); else RETURN(s_forward);
	}
}
