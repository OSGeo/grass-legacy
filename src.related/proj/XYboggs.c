#ifndef lint
static char *SCCSID = "@(#)XYboggs.c	USGS v.3.1";
#endif
/*  Boggs Eumorphic */
# include	"projects.h"
# define NITER	20
# define EPS	1e-7
# define ONETOL 1.000001
# define FXC	2.00276
# define FXC2	1.11072
# define FYC	0.49931
# define FYC2	1.41421356237309504880
#undef INV_CODE
#ifdef FOR_CODE
FORWARD(s_forward); /* spheroid */
	double theta, th1, c;
	int i;

	theta = phi;
	if (fabs(fabs(phi) - HALFPI) < EPS)
		x = 0.;
	else {
		c = sin(theta) * PI;
		for (i = NITER; i; --i) {
			theta -= th1 = (theta + sin(theta) - c) /
				(1. + cos(theta));
			if (fabs(th1) < EPS) break;
		}
		theta *= 0.5;
		x = FXC * lam / (1. / cos(phi) + FXC2 / cos(theta));
	}
	y = FYC * (phi + FYC2 * sin(theta));
	return (xy);
}
#else
NULL_FORWARD(s_forward);
#endif
#ifdef INV_CODE
INVERSE(s_inverse); /* spheroid */
}
#else
NULL_INVERSE(s_inverse);
#endif
ENTRY(boggs) { if (inverse) RETURN(s_inverse); else RETURN(s_forward); }
