#ifndef lint
static char *SCCSID = "@(#)UVmbtfpq.c	USGS v.3.1";
#endif
/*  McBryde-Thomas Flat-Polar Quartic */
# include	"projects.h"
# define NITER	20
# define EPS	1e-7
# define ONETOL 1.000001
# define C	1.70710678118654752440
# define RC	0.58578643762690495119
# define FYC	1.87475828462269495505
# define RYC	0.53340209679417701685
# define FXC	0.31245971410378249250
# define RXC	3.20041258076506210122
#ifdef FOR_CODE
FORWARD(s_forward); /* spheroid */
	double th1, c;
	int i;

	c = C * sin(phi);
	for (i = NITER; i; --i) {
		phi -= th1 = (sin(.5*phi) + sin(phi) - c) /
			(.5*cos(.5*phi)  + cos(phi));
		if (fabs(th1) < EPS) break;
	}
	x = FXC * lam * (1.0 + 2. * cos(phi)/cos(0.5 * phi));
	y = FYC * sin(0.5 * phi);
	return (xy);
}
#else
NULL_FORWARD(s_forward);
#endif
#ifdef INV_CODE
INVERSE(s_inverse); /* spheroid */
	double t;

	phi = RYC * y;
	if (fabs(phi) > 1.) {
		if (fabs(phi) > ONETOL)	I_ERROR
		else if (phi < 0.) { t = -1.; phi = -PI; }
		else { t = 1.; phi = PI; }
	} else
		phi = 2. * asin(t = phi);
	lam = RXC * x / (1. + 2. * cos(phi)/cos(0.5 * phi));
	phi = RC * (t + sin(phi));
	if (fabs(phi) > 1.)
		if (fabs(phi) > ONETOL)	I_ERROR
		else			phi = phi < 0. ? -HALFPI : HALFPI;
	else
		phi = asin(phi);
	return (lp);
}
#else
NULL_INVERSE(s_inverse);
#endif
ENTRY(mbtfpq) {
	if (inverse) RETURN(s_inverse); else RETURN(s_forward);
}
