static char *SCCSID = "@(#)poly.c	AMG v.1.2";
/* Polyconic projection */
# define	PROJ_MOD
# include	"projects.h"
struct _PROJ { PHEAD;
	double	PHI0, E, E0[E_MAX], ML0;
};
# define TOL	1.e-7
# define CONV	1e-10
# define N_ITER	10
# define ml0 	proj->ML0
FORWARD(e_forward); /* ellipsoid */
	double  ml, ms, sp;

	if (fabs(phi) <= TOL) {
		x = lam;
		y = -ml0;
	} else {
		ml = mlfn_(e0, phi);
		sp = sin(phi);
		ms = msfn_(e, sp, cos(phi)) / sp;
		x = ms * sin(lam *= sp);
		y = (ml - ml0) + ms * (1. - cos(lam));
	}
	return (&xy);
}
INVERSE(e_inverse); /* ellipsoid */
	double c;

	y += ml0;
	if (fabs(y) <= TOL) {
		lam = x;
		phi = 0.;
	} else {
		if((phi = phi4_(es, e0, y, y * y + x * x)) == HUGE)
			ERROR;
		c = sin(phi);
		lam = asin(x * tan(phi) * sqrt(1. - es * c * c)) / sin(phi);
	}
	return (&lp);
}
FORWARD(s_forward); /* spheroid */
	double  cot, E;

	if (fabs(phi) <= TOL) {
		x = lam;
		y = ml0;
	} else {
		cot = 1. / tan(phi);
		x = sin(E = lam * sin(phi)) * cot;
		y = phi - phi0 + cot * (1. - cos(E));
	}
	return (&xy);
}
INVERSE(s_inverse); /* spheroid */
	double B, dphi, tp;
	int i;

	if (fabs(y = phi0 + y) <= TOL) {
		lam = x;
		phi = 0.;
	} else {
		phi = y;
		B = x * x + y * y;
		i = N_ITER;
		do {
			tp = tan(phi);
			phi -= (dphi = (y * (phi * tp + 1.) - phi -
				.5 * ( phi * phi + B) * tp) /
				((phi - y) / tp - 1.));
		} while (fabs(dphi) > CONV && --i);
		if (! i) ERROR;
		lam = asin(x * tan(phi)) / sin(phi);
	}
	return (&lp);
}
ENTRY(poly) {
	if (! proj) return (sizeof(struct _PROJ));
	if (comset(I_ALL, proj, param)) ERROR;
	phi0 = *(*param)("rlat_0", "");
	if (es) {
		proj->forward = e_forward;
		proj->inverse = e_inverse;
		e = sqrt(es);
		enfn_(e0, es);
		ml0 = mlfn_(e0, phi0);
	} else {
		proj->forward = s_forward;
		proj->inverse = s_inverse;
		ml0 -= phi0;
	}
	return (1);
}
