static char *SCCSID = "@(#)phi4_.c	AMG v.1.2";
/* determine latitude */
# include "projects.h"

# define N_ITER 15
# define TOL 1.e-10

	double
phi4_(eccnts, e0, a, b) double eccnts, *e0, a, b; {
	double c, phi, ph2, ph4, ph6, s2ph, ml, mlb, mlp, dphi;
	int i;

	phi = a;
	i = N_ITER;
	do {
		s2ph = sin(phi);
		c = tan(phi) * sqrt(1. - eccnts * s2ph * s2ph);
		s2ph = sin(ph2 = phi + phi);
		ph4 = ph2 + ph2;
		ph6 = ph4 * ph2;
		ml = *e0 * phi - e0[1] * s2ph + e0[2] * sin(ph4)
			- e0[3] * sin(ph6);
		mlb = ml * ml + b;
		mlp = *e0 - 2. * e0[1] * cos(ph2) + 4. * e0[2] * cos(ph4)
			- 6. * e0[3] * cos(ph6);
		phi += ( dphi =
		   ( 2. * ml + c * mlb - 2. * a * (c * ml + 1.) ) / (
		   eccnts * s2ph * (mlb - 2. * a * ml) / (2. * c) +
		   2. * (a - ml) * (c * mlp - 2. / s2ph) - 2. * mlp ));
	} while (fabs(dphi) > TOL && --i);
	return ( i ? phi : HUGE );
}
