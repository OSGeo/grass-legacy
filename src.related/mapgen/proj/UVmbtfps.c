#ifndef lint
static char *SCCSID = "@(#)UVmbtfps.c	USGS v.3.1";
#endif
/*  McBryde-Thomas Flat-Polar Sinusoidal */
# include	"projects.h"
# define NITER	20
# define EPS	1e-7
# define ONETOL 1.000001
# define C	1.78539816339744830961
# define RC	0.56009915351155737591
# define FYC	0.91659627441275150748
# define RYC	1.09099286994231339007
# define FXC	0.61106418294183433832
# define RXC	1.63648930491347008510
#ifdef FOR_CODE
FORWARD(s_forward); /* spheroid */
	double th1, c;
	int i;

	c = C * sin(phi);
	for (i = NITER; i; --i) {
		phi -= th1 = (0.5 * phi + sin(phi) - c )/(0.5 + cos(phi));
		if (fabs(th1) < EPS) break;
	}
	x = FXC * lam * (0.5 + cos(phi));
	y = FYC * phi;
	return (xy);
}
#else
NULL_FORWARD(s_forward);
#endif
#ifdef INV_CODE
INVERSE(s_inverse); /* spheroid */
	double t;

	phi = RYC * y;
	lam = RXC * x / (0.5 + cos(phi));
	phi = RC * (0.5 * phi + sin(phi));
	if (fabs(phi) > 1.)
		if (fabs(phi) > ONETOL)	I_ERROR
		else			phi = phi > 0. ? HALFPI : - HALFPI;
	else
		phi = asin(phi);
	return (lp);
}
#else
NULL_INVERSE(s_inverse);
#endif
ENTRY(mbtfps) { if (inverse) RETURN(s_inverse); else RETURN(s_forward); }
