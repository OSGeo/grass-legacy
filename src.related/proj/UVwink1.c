#ifndef lint
static char *SCCSID = "@(#)UVwink1.c	USGS v.3.1";
#endif
/*  Winkel I */
# include	"projects.h"
	static double
cosphi1;
#ifdef FOR_CODE
FORWARD(s_forward); /* spheroid */
	x = .5 * lam * (cosphi1 + cos(phi));
	y = phi;
	return (xy);
}
#else
NULL_FORWARD(s_forward);
#endif
#ifdef INV_CODE
INVERSE(s_inverse); /* spheroid */
	phi = y;
	lam = 2. * x / (cosphi1 + cos(phi));
	return (lp);
}
#else
NULL_INVERSE(s_inverse);
#endif
ENTRY(wink1) {
	cosphi1 = cos(*(*param)("rlat_ts",""));
	if (inverse) RETURN(s_inverse); else RETURN(s_forward);
}
