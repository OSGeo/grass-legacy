#ifndef lint
static char *SCCSID = "@(#)UVquau.c	USGS v.3.1";
#endif
/*  Quartic Authalic */
# include	"projects.h"
#define ONEEPS	1.0000001
#ifdef FOR_CODE
FORWARD(s_forward); /* spheroid */
	x = lam * cos(phi);
	x /= cos(phi *= 0.5);
	y = 2. * sin(phi);
	return (xy);
}
#else
NULL_FORWARD(s_forward);
#endif
#ifdef INV_CODE
INVERSE(s_inverse); /* spheroid */
	if (fabs(y *= .5) >= 1.)
		if (fabs(y) > ONEEPS)	I_ERROR
		else		phi = y < 0. ? PI : -PI;
	else
		phi = 2. * asin(y);
	if ((lam = cos(phi)) == 0.)
		lam = 0.;
	else
		lam = x * cos(.5 * phi) / lam;
	return (lp);
}
#else
NULL_INVERSE(s_inverse);
#endif
ENTRY(quau) { if (inverse) RETURN(s_inverse); else RETURN(s_forward); }
