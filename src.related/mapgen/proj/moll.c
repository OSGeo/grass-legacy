static char *SCCSID = "@(#)moll.c	AMG v.1.1";
/* Mollweide projection */
# define	PROJ_MOD
# include	"projects.h"
struct _PROJ { PHEAD; };
# define C1R	.90031631615710606956
# define C2R	1.41421356237309504880
# define EPS	1e-15
# define EPS10	1e-10
# define NITER	10
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
	return (&xy);
}
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
	return (&lp);
}
ENTRY(moll) {
	if (! proj) return (sizeof(struct _PROJ));
	if (comset(I_LAM0 + I_XY, proj, param)) ERROR;
	proj->forward = s_forward;
	proj->inverse = s_inverse;
	return (1);
}
