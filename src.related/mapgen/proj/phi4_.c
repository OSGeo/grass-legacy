#ifndef lint
static char *SCCSID = "@(#)phi4_.c	USGS v.3.2";
#endif
/* determine latitude for polyconic */
# include "projects.h"

# define N_ITER 20
# define TOL 1.e-12
	double
phi4_(phi_a, b) double phi_a, b; {
	double c, sp, cp, Phi, s2ph, ml, mlb, mlp, dPhi;
	int i;

	for (Phi = phi_a, i = N_ITER; i ; --i) {
		sp = sin(Phi);
		s2ph = sp * ( cp = cos(Phi));
		if (fabs(cp) < TOL)
			break;
		c = sp * (mlp = sqrt(1. - es * sp * sp)) / cp;
		ml = mlfn_(Phi, sp, cp);
		mlb = ml * ml + b;
		mlp = one_es / (mlp * mlp * mlp);
		Phi += ( dPhi =
			( ml + ml + c * mlb - 2. * phi_a * (c * ml + 1.) ) / (
			es * s2ph * (mlb - 2. * phi_a * ml) / c +
			2.* (phi_a - ml) * (c * mlp - 1. / s2ph) - mlp - mlp ));
		if (fabs(dPhi) <= TOL)
			return Phi;
	}
	return HUGE;
}
