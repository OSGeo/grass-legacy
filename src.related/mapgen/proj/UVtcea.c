#ifndef lint
static char *SCCSID = "@(#)UVtcea.c	USGS v.3.1";
#endif
/*  Transverse Cylindrical Equal Area */
# include	"projects.h"
	static double
rok, rtk;
#ifdef FOR_CODE
FORWARD(s_forward); /* spheroid */
	x = rok * cos(phi) * sin(lam);
	y = rtk * (atan2(tan(phi), cos(lam)) - phi0);
	return (xy);
}
#else
NULL_FORWARD(s_forward);
#endif
#ifdef INV_CODE
INVERSE(s_inverse); /* spheroid */
	double t;

	y = y / rtk + phi0;
	x /= rok;
	t = sqrt(1. - x * x);
	phi = asin(t * sin(y));
	lam = atan2(x, t * cos(y));
	return (lp);
}
#else
NULL_INVERSE(s_inverse);
#endif
ENTRY(tcea) {
	double k_0;

	if (!(k_0 = *(*param)("dk", "1."))) E_ERROR;
	rok = a / k_0;
	rtk = a * k_0;
	if (inverse) RETURN(s_inverse); else RETURN(s_forward);
}
