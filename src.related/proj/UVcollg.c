#ifndef lint
static char *SCCSID = "@(#)UVcollg.c	USGS v.3.1";
#endif
/*  Collignon */
# include	"projects.h"
#define FXC	1.12837916709551257390
#define FYC	1.77245385090551602729
#define ONEEPS	1.0000001
#ifdef FOR_CODE
FORWARD(s_forward); /* spheroid */
	if ((y = 1. - sin(phi)) <= 0.)
		y = 0.;
	else
		y = sqrt(y);
	x = FXC * lam * y;
	y = FYC * (1. - y);
	return (xy);
}
#else
NULL_FORWARD(s_forward);
#endif
#ifdef INV_CODE
INVERSE(s_inverse); /* spheroid */
	phi = y / FYC - 1.;
	if (fabs(phi = 1. - phi * phi) < 1.)
		phi = asin(phi);
	else if (fabs(phi) > ONEEPS) I_ERROR
	else	phi = phi < 0. ? -HALFPI : HALFPI;
	if ((lam = 1. - sin(phi)) <= 0.)
		lam = 0.;
	else
		lam = x / (FXC * sqrt(lam));
	return (lp);
}
#else
NULL_INVERSE(s_inverse);
#endif
ENTRY(collg) { if (inverse) RETURN(s_inverse); else RETURN(s_forward); }
