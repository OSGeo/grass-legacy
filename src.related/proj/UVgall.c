#ifndef lint
static char *SCCSID = "@(#)UVgall.c	USGS v.3.1";
#endif
/*  Gall (Stereographic) */
# include	"projects.h"
#define YF	1.70710678118654752440
#define XF	0.70710678118654752440
#define RYF	0.58578643762690495119
#define RXF	1.41421356237309504880
#ifdef FOR_CODE
FORWARD(s_forward); /* spheroid */
	x = XF * lam;
	y = YF * tan(.5 * phi);
	return (xy);
}
#else
NULL_FORWARD(s_forward);
#endif
#ifdef INV_CODE
INVERSE(s_inverse); /* spheroid */
	lam = RXF * x;
	phi = 2. * atan(y * RYF);
	return (lp);
}
#else
NULL_INVERSE(s_inverse);
#endif
ENTRY(gall) {
	if (inverse) RETURN(s_inverse); else RETURN(s_forward);
}
