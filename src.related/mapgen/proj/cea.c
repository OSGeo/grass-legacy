static char *SCCSID = "@(#)cea.c	AMG v.1.3";
/* Equal Area Cylindrical projection */
# define	PROJ_MOD
# include	"projects.h"
struct _PROJ { PHEAD;
	double E, K0, AE[AE_MAX], QP;
};
# define k0	proj->K0
# define ae	proj->AE
# define qp	proj->QP
# define EPS	1e-10
FORWARD(e_forward); /* spheroid */
	x = k0 * lam;
	y = .5 * qsfn_(e, sin(phi)) / k0;
	return (&xy);
}
FORWARD(s_forward); /* spheroid */
	x = k0 * lam;
	y = sin(phi) / k0;
	return (&xy);
}
INVERSE(e_inverse); /* spheroid */
	phi = authlat(ae, asin( 2. * y * k0 / qp));
	lam = x / k0;
	return (&lp);
}
INVERSE(s_inverse); /* spheroid */
	double t;

	if ((t = fabs(y *= k0)) - EPS <= 1.) {
		if (t >= 1.)
			phi = y < 0. ? -HALFPI : HALFPI;
		else
			phi = asin(y);
		lam = x / k0;
	} else ERROR;
	return (&lp);
}
ENTRY(cea) {
	double t;

	if (! proj) return (sizeof(struct _PROJ));
	if (comset(I_ALL, proj, param)) ERROR;
	if ((k0 = cos(t = *(*param)("rlat_ts", ""))) <= 0.) ERROR;
	if (es == 0.) {
		proj->forward = s_forward;
		proj->inverse = s_inverse;
	} else {
		proj->forward = e_forward;
		proj->inverse = e_inverse;
		t = sin(t);
		k0 /= sqrt(1. - es * t * t);
		e = sqrt(es);
		authset(ae, es);
		qp = qsfn_(e, 1.);
	}
	return 1;
}
