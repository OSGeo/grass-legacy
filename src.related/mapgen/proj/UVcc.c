#ifndef lint
static char *SCCSID = "@(#)UVcc.c	USGS v.3.1";
#endif
/* Mercator projection */
# include	"projects.h"
	static double
ap;
# define EPS10 1.e-10
#ifdef FOR_CODE
FORWARD(s_forward); /* spheroid */
	if (fabs(fabs(phi) - HALFPI) <= EPS10) F_ERROR;
	x = lam;
	y = tan(phi);
	return (xy);
}
#else
NULL_FORWARD(s_forward);
#endif
#ifdef INV_CODE
INVERSE(s_inverse); /* spheroid */
	phi = atan(y);
	lam = x;
	return (lp);
}
#else
NULL_INVERSE(s_inverse);
#endif
ENTRY(cc) { if (inverse) RETURN(s_inverse); else RETURN(s_forward); }
