static char *SCCSID = "@(#)eck6.c	AMG v.1.1";
/* Eckert VI projection */
# define	PROJ_MOD
# include	"projects.h"
struct _PROJ { PHEAD; };
# define C1R	.44101277172455148219
# define C2R	.88202554344910296438
# define RC2R	1.13375401361911319568
# define C5	2.57079632679489661922
# define RC5	.38898452964834271062
# define EPS	1e-10
# define EPS10	1e-10
# define NITER	10
	static double
theta(ph) double ph; {
	double th, dth;
	int i;

	ph = C5 * sin(th = ph);
	for (i = NITER; i ; --i) {
		th += ( dth = (ph - th - sin(th)) / (1. + cos(th)) );
		if (fabs(dth) < EPS)
			break;
	}
	return th;
}
FORWARD(s_forward); /* spheroid */
	double th;

	x = C1R * lam * (1. + cos(th = theta(phi)));
	y = C2R * th;
	return (&xy);
}
INVERSE(s_inverse); /* spheroid */
	double s;

	y *= RC2R;
	if ((s = fabs(y = (y + sin(y)) * RC5)) < 1.)
		lam = x / (C1R * (1. + cos(phi = asin(y))));
	else if ((s - EPS10) > 1.) ERROR;
	else {
		lam = 0.;
		phi = y < 0. ? -HALFPI : HALFPI;
	}
	return (&lp);
}
ENTRY(eck6) {
	if (! proj) return (sizeof(struct _PROJ));
	if (comset(I_LAM0 + I_XY, proj, param)) ERROR;
	proj->forward = s_forward;
	proj->inverse = s_inverse;
	return (1);
}
