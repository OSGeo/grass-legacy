static char *SCCSID = "@(#)bonne.c	AMG v.1.1";
/* Bonne projection */
# define	PROJ_MOD
# include	"projects.h"
struct _PROJ { PHEAD;
	double	PHI1, CPHI1, E0[E_MAX], U0[U_MAX], AM1, M1;
};
# define u0	proj->U0
# define cphi1	proj->CPHI1
# define am1	proj->AM1
# define m1	proj->M1
# define EPS10	1e-10
FORWARD(e_forward); /* ellipsoid */
	double rh, E;

	rh = am1 + m1 - mlfn_(e0, phi);
	E = sin(phi);
	E = cos(phi) * lam / (rh * sqrt(1. - es * E * E));
	x = rh * sin(E);
	y = am1 - rh * cos(E);
	return (&xy);
}
INVERSE(e_inverse); /* ellipsoid */
	double s, rh;

	rh = hypot(x, y = am1 - y);
	phi = mufn_(u0, (am1 + m1 - rh) / e0[0]);
	if ((s = fabs(phi)) < HALFPI) {
		s = sin(phi);
		lam = rh * atan2(x, y) *
		   sqrt(1. - es * s * s) / cos(phi);
	} else if (fabs(s - HALFPI) <= EPS10)
		lam = 0.;
	else ERROR;
	return (&lp);
}
FORWARD(s_forward); /* spheroid */
	double E, rh;

	rh = cphi1 + phi1 - phi;
	if (fabs(rh) > EPS10) {
		x = rh * sin(E = lam * cos(phi) / rh);
		y = cphi1 - rh * cos(E);
	} else
		x = y = 0.;
	return (&xy);
}
INVERSE(s_inverse); /* spheroid */
	double rh;

	rh = hypot(x, y = cphi1 - y);
	phi = cphi1 + phi1 - rh;
	if (fabs(phi) > HALFPI) ERROR;
	if (fabs(fabs(phi) - HALFPI) <= EPS10)
		lam = 0.;
	else
		lam = rh * atan2(x, y) / cos(phi);
	return (&lp);
}
ENTRY(bonne) {
	if (! proj) return (sizeof(struct _PROJ));
	if (comset(I_ALL, proj, param)) ERROR;
	phi1 = *(*param)("rlat_0", "40.");
	if (fabs(phi1) < EPS10) ERROR;
	if (es) {
		proj->forward = e_forward;
		proj->inverse = e_inverse;
		enfn_(e0, es);
		mifn_(u0, es);
		m1 = mlfn_(e0, phi1);
		am1 = sin(phi1);
		am1 = cos(phi1) / (sqrt(1. - es * am1 * am1) * sin(phi1));
	} else {
		proj->forward = s_forward;
		proj->inverse = s_inverse;
		if (fabs(phi1) + EPS10 >= HALFPI)
			cphi1 = 0.;
		else
			cphi1 = 1. / tan(phi1);
	}
	return (1);
}
