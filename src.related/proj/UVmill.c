#ifndef lint
static char *SCCSID = "@(#)UVmill.c	USGS v.3.1";
#endif
/* Miller Cylindrical projection */
# include	"projects.h"
#ifdef FOR_CODE
FORWARD(s_forward); /* spheroid */
	x = lam;
	y = log(tan(FORTPI + phi * .4)) * 1.25;
	return (xy);
}
#else
NULL_FORWARD(s_forward);
#endif
#ifdef INV_CODE
INVERSE(s_inverse); /* spheroid */
	lam = x;
	phi = 2.5 * (atan(exp(.8 * y)) - FORTPI);
	return (lp);
}
#else
NULL_INVERSE(s_inverse);
#endif
ENTRY(mill) { if (inverse) RETURN(s_inverse); else RETURN(s_forward); }
