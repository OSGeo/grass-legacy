#ifndef lint
static char *SCCSID = "@(#)XYvandg2.c	USGS v.3.3";
#endif
/*  Van der Grinten II Projection */
# include	"projects.h"
# define TOL	1e-10
# define TWORPI	0.63661977236758134308
	static int
vdg3;
#ifdef FOR_CODE
FORWARD(s_forward); /* spheroid */
	double x1, at, bt, ct;

	bt = fabs(TWORPI * phi);
	if ((ct = 1. - bt * bt) < 0.)
		ct = 0.;
	else
		ct = sqrt(ct);
	if (fabs(lam) < TOL) {
		x = 0.;
		y = PI * (phi < 0. ? -bt : bt) / (1. + ct);
	} else {
		at = 0.5 * fabs(PI / lam - lam / PI);
		if (vdg3) {
			x1 = bt / (1. + ct);
			x = PI * (sqrt(at * at + 1. - x1 * x1) - at);
			y = PI * x1;
		} else {
			x1 = (ct * sqrt(1. + at * at) - at * ct * ct) /
				(1. + at * at * bt * bt);
			x = PI * x1;
			y = PI * sqrt(1. - x1 * (x1 + 2. * at) + TOL);
		}
		if ( lam < 0.) x = -x;
		if ( phi < 0.) y = -y;
	}
	return (xy);
}
#else
NULL_FORWARD(s_forward);
#endif
NULL_FORWARD(s_inverse);
ENTRY(vandg2) {
	vdg3 = 0;
	if (inverse) RETURN(s_inverse); else RETURN(s_forward);
}
ENTRY(vandg3) {
	vdg3 = 1;
	if (inverse) RETURN(s_inverse); else RETURN(s_forward);
}
