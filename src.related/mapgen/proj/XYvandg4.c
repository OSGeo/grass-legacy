#ifndef lint
static char *SCCSID = "@(#)XYvandg4.c	USGS v.3.4";
#endif
/*  Van der Grinten IV Projection */
# include	"projects.h"
# define TOL	1e-10
# define TWORPI	0.63661977236758134308
#ifdef FOR_CODE
FORWARD(s_forward); /* spheroid */
	double x1, t, bt, ct, ft, bt2, ct2, ft2, dt, dt2;

	if (fabs(phi) < TOL) {
		x = lam;
		y = 0.;
	} else if (fabs(lam) < TOL || fabs(fabs(phi) - HALFPI) < TOL) {
		x = 0.;
		y = phi;
	} else {
		bt = fabs(TWORPI * phi);
		bt2 = bt * bt;
		ct = 0.5 * (bt * (8. - bt * (2. + bt2)) - 5.)
			/ (bt2 * (bt - 1.));
		ct2 = ct * ct;
		dt = TWORPI * lam;
		dt = dt + 1. / dt;
		dt = sqrt(dt * dt - 4.);
		if ((fabs(lam) - HALFPI) < 0.) dt = -dt;
		dt2 = dt * dt;
		x1 = bt + ct; x1 *= x1;
		t = bt + 3.*ct;
		ft = x1 * (bt2 + ct2 * dt2 - 1.) + (1.-bt2) * (
			bt2 * (t * t + 4. * ct2) +
			ct2 * (12. * bt * ct + 4. * ct2) );
		x1 = (dt*(x1 + ct2 - 1.) + 2.*sqrt(ft)) /
			(4.* x1 + dt2);
		x = HALFPI * x1;
		y = HALFPI * sqrt(1. + dt * fabs(x1) - x1 * x1);
		if (lam < 0.) x = -x;
		if (phi < 0.) y = -y;
	}
	return (xy);
}
#else
NULL_FORWARD(s_forward);
#endif
NULL_FORWARD(s_inverse);
ENTRY(vandg4) { if (inverse) RETURN(s_inverse); else RETURN(s_forward); }
