static char *SCCSID = "@(#)eck4.c	AMG v.1.1";
/* Eckert IV projection */
# define	PROJ_MOD
# include	"projects.h"
struct _PROJ { PHEAD; };
# define C1	.42223820031577120149
# define C2	1.32650042817700232218
# define RC2	.75386330736002178205
# define C3	3.57079632679489661922
# define RC3	.28004957675577868795
# define EPS	1e-10
# define EPS10	1e-10
# define NITER	10
	static double
theta(ph) double ph; {
	double th, dth, s, c;
	int i;

	th = .5 * ph;
	ph = C3 * sin(ph);
	for (i = NITER; i ; --i) {
		c = cos(th);
		s = sin(th);
		th += ( dth =
		   (ph - th - (c + 2.) * s) / (2. * c * (1. + c)) );
		if (fabs(dth) < EPS)
			break;
	}
	return th;
}
FORWARD(s_forward); /* spheroid */
	x = C1 * lam * (1. + cos(phi = theta(phi)));
	y = C2 * sin(phi);
	return (&xy);
}
INVERSE(s_inverse); /* spheroid */
	double c, s;

	if ((s = fabs(y *= RC2)) < 1.) {
		c = cos(s = asin(y));
		phi = asin((s + y * (c + 2.)) * RC3);
		lam = x / (C1 * (1. + c));
	} else if ((s - EPS10) > 1. ) ERROR;
	else {
		lam = 0.;
		phi = y < 0. ? - HALFPI : HALFPI;
	}
	return (&lp);
}
ENTRY(eck4) {
	if (! proj) return (sizeof(struct _PROJ));
	if (comset(I_LAM0 + I_XY, proj, param)) ERROR;
	proj->forward = s_forward;
	proj->inverse = s_inverse;
	return (1);
}
