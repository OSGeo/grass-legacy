#ifndef lint
static char *SCCSID = "@(#)UVeck3.c	USGS v.3.1";
#endif
/*  Eckert III */
# include	"projects.h"
# define XF	.42223820031577120149
# define RXF	2.36833142821314873781
# define YF	.84447640063154240298
# define RYF	1.18416571410657436890
#ifdef FOR_CODE
FORWARD(s_forward); /* spheroid */
	if (fabs(x = phi / HALFPI) >= 1.)
		x = XF * lam;
	else
		x = XF * (1. + sqrt(1. - x*x)) * lam;
	y = YF * phi;
	return (xy);
}
#else
NULL_FORWARD(s_forward);
#endif
#ifdef INV_CODE
INVERSE(s_inverse); /* spheroid */
	double t;

	lam = x * RXF;
	if (fabs(t = (phi = RYF * y) / HALFPI) < 1.)
		lam /= 1. + sqrt(1. - t*t);
	return (lp);
}
#else
NULL_INVERSE(s_inverse);
#endif
ENTRY(eck3) {
	if (inverse) RETURN(s_inverse); else RETURN(s_forward);
}
