#ifndef lint
static char *SCCSID = "@(#)UVmoll.c	USGS v.3.1";
#endif
/* Mollweide projection */
# include	"projects.h"
# define C1R	.90031631615710606956
# define C2R	1.41421356237309504880
# define EPS	1e-15
# define EPS10	1e-10
# define NITER	10
#ifdef FOR_CODE
	static double
theta(ph) double ph; {
	double th, dth;
	int i;

	ph = PI * sin(th = ph);
	for (i = NITER; i ; --i) {
		th += ( dth = (ph - th - sin(th)) / (1. + cos(th)) );
		if (fabs(dth) < EPS)
			break;
	}
	return (.5 * th);
}
FORWARD(s_forward); /* spheroid */
	double th;

	x = C1R * lam * cos(th = theta(phi));
	y = C2R * sin(th);
	return (xy);
}
#else
NULL_FORWARD(s_forward);
#endif
#ifdef INV_CODE
INVERSE(s_inverse); /* spheroid */
	double th, s;

	if ((s = fabs(th = y / C2R)) < 1.) {
		lam = x / (C1R * cos(th = asin(th)));
		th += th;
		phi = asin((th + sin(th)) / PI);
	} else if ((s - EPS10) > 1.)
		lam = phi = HUGE;
	else {
		lam = 0.;
		phi = th < 0. ? -HALFPI : HALFPI;
	}
	return (lp);
}
#else
NULL_INVERSE(s_inverse);
#endif
ENTRY(moll) { if (inverse) RETURN(s_inverse); else RETURN(s_forward); }
