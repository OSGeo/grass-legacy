#ifndef lint
static char *SCCSID = "@(#)UVputp2.c	USGS v.3.1";
#endif
/*  Putnins P2' */
# include	"projects.h"
# define NITER	20
# define EPS	1e-7
# define ONETOL 1.000001
# define C	2.96042050617763413905
# define RC	0.33778985043282124554
# define FYC	1.56548
# define RYC	0.63878171551217517949
# define FXC	0.86310
# define RXC	1.15861429730042868729
#ifdef FOR_CODE
FORWARD(s_forward); /* spheroid */
	double th1, c;
	int i;

	c = C * sin(phi);
	for (i = NITER; i; --i) {
		phi -= th1 = (phi + sin(phi) - c) / (1. + cos(phi));
		if (fabs(th1) < EPS) break;
	}
	x = FXC * lam * cos(phi *= 0.5);
	y = FYC * sin(phi);
	return (xy);
}
#else
NULL_FORWARD(s_forward);
#endif
#ifdef INV_CODE
INVERSE(s_inverse); /* spheroid */

	phi = RYC * y;
	if (fabs(phi) > 1.)
		if (fabs(phi) > ONETOL)	I_ERROR
		else			phi > 0. ? HALFPI : - HALFPI;
	else
		phi = asin(phi);
	lam = RXC * x / cos(phi);
	phi += phi;
	phi = RC * (phi + sin(phi));
	if (fabs(phi) > 1.)
		if (fabs(phi) > ONETOL)	I_ERROR
		else			phi > 0. ? HALFPI : - HALFPI;
	else
		phi = asin(phi);
	return (lp);
}
#else
NULL_INVERSE(s_inverse);
#endif
ENTRY(putp2) { if (inverse) RETURN(s_inverse); else RETURN(s_forward); }
