static char *SCCSID = "@(#)sinu.c	AMG v.1.1";
/* Sinusoidal Projection */
# define	PROJ_MOD
# include	"projects.h"
struct _PROJ { PHEAD;
	double	E0[E_MAX], U0[U_MAX];
};
# define u0	proj->U0
# define EPS10	1e-10
FORWARD(e_forward); /* ellipsoid */
	double s;

	y = mlfn_(e0, phi);
	s = sin(phi);
	x = lam * cos(phi) / sqrt(1. - es * s * s);
	return (&xy);
}
INVERSE(e_inverse); /* ellipsoid */
	double s;

	if ((s = fabs(phi = mufn_(u0, y / e0[0]))) < HALFPI) {
		s = sin(phi);
		lam = x * sqrt(1. - es * s * s) / cos(phi);
	} else if ((s - EPS10) < HALFPI)
		lam = 0.;
	else ERROR;
	return (&lp);
}
FORWARD(s_forward); /* spheroid */
	x = lam * cos(y = phi);
	return (&xy);
}
INVERSE(s_inverse); /* spheroid */
	double s;

	if ((s = fabs(phi = y)) < HALFPI)
		lam = x / cos(phi);
	else if ((s - EPS10) < HALFPI)
		lam = 0.;
	else ERROR;
	return (&lp);
}
ENTRY(sinu) {
	if (! proj) return (sizeof(struct _PROJ));
	if (comset(I_ALL, proj, param)) ERROR;
	if (es) {
		proj->forward = e_forward;
		proj->inverse = e_inverse;
		enfn_(e0, es);
		mifn_(u0, es);
	} else {
		proj->forward = s_forward;
		proj->inverse = s_inverse;
	}
	return (1);
}
