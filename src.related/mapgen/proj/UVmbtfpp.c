#ifndef lint
static char *SCCSID = "@(#)UVmbtfpp.c	USGS v.3.1";
#endif
/*  McBride-Thomas Flat-Polar Parabolic */
# include	"projects.h"
# define CS	.95257934441568037152
# define FXC	.92582009977255146156
# define FYC	3.40168025708304504493
# define C23	.66666666666666666666
# define C13	.33333333333333333333
# define ONEEPS	1.0000001
#ifdef FOR_CODE
FORWARD(s_forward); /* spheroid */
	phi = asin(CS * sin(phi));
	x = FXC * lam * (2. * cos(C23 * phi) - 1.);
	y = FYC * sin(C13 * phi);
	return (xy);
}
#else
NULL_FORWARD(s_forward);
#endif
#ifdef INV_CODE
INVERSE(s_inverse); /* spheroid */
	phi = y / FYC;
	if (fabs(phi) >= 1.) {
		if (fabs(phi) > ONEEPS)	I_ERROR
		else	phi = (phi < 0.) ? -HALFPI : HALFPI;
	} else
		phi = asin(phi);
	lam = x / ( FXC * (2. * cos(C23 * (phi *= 3.)) - 1.) );
	if (fabs(phi = sin(phi) / CS) >= 1.) {
		if (fabs(phi) > ONEEPS)	I_ERROR
		else	phi = (phi < 0.) ? -HALFPI : HALFPI;
	} else
		phi = asin(phi);
	return (lp);
}
#else
NULL_INVERSE(s_inverse);
#endif
ENTRY(mbtfpp) { if (inverse) RETURN(s_inverse); else RETURN(s_forward); }
