#ifndef lint
static char *SCCSID = "@(#)XYhammer.c	USGS v.3.4";
#endif
/*  Hammer & Eckert-Greifendorff Projections */
# include	"projects.h"
	static double
W;
# undef INV_CODE
#ifdef FOR_CODE
FORWARD(s_forward); /* spheroid */
	double cosphi, d;

	d = sqrt(2./(1. + (cosphi = cos(phi)) * cos(lam *= W)));
	x = d * cosphi * sin(lam) / W;
	y = d * sin(phi);
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
ENTRY(hammer) {
	if ((W = fabs(*(*param)("dW", "0.5"))) == 0.) {
		emess(1,"W == 0.");
		E_ERROR;
	}
	if (inverse) RETURN(s_inverse); else RETURN(s_forward);
}
