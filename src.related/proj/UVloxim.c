#ifndef lint
static char *SCCSID = "@(#)UVloxim.c	USGS v.3.2";
#endif
/*  Loximuthal Projection */
# include	"projects.h"
# define EPS	1e-8
	static double
phi1, cosphi1, tanphi1;
#ifdef FOR_CODE
FORWARD(s_forward); /* spheroid */
	y = phi - phi1;
	if (fabs(y) < EPS)
		x = lam * cosphi1;
	else {
		x = FORTPI + 0.5 * phi;
		if (fabs(x) < EPS || fabs(fabs(x) - HALFPI) < EPS)
			x = 0.;
		else
			x = lam * y / log( tan(x) / tanphi1 );
	}
	return (xy);
}
#else
NULL_FORWARD(s_forward);
#endif
#ifdef INV_CODE
INVERSE(s_inverse); /* spheroid */
	phi = y + phi1;
	if (fabs(y) < EPS)
		lam = x / cosphi1;
	else
		if (fabs( lam = FORTPI + 0.5 * phi ) < EPS ||
			fabs(fabs(lam) - HALFPI) < EPS)
			lam = 0.;
		else
			lam = x * log( tan(lam) / tanphi1 ) / y ;
	return (lp);
}
#else
NULL_INVERSE(s_inverse);
#endif
ENTRY(loxim) {
	phi1 = *(*param)("rlat_1", "0");
	if ((cosphi1 = cos(phi1)) < EPS) {
		emess(-1,"| lat_1 | ~= 90");
		E_ERROR;
	}
	tanphi1 = tan(FORTPI + 0.5 * phi1);
	if (inverse) RETURN(s_inverse); else RETURN(s_forward);
}
