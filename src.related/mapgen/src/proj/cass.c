static char *SCCSID = "@(#)cass.c	AMG v.1.1";
/* Cassini projection */
# define	PROJ_MOD
# include	"projects.h"
struct _PROJ { PHEAD;
	double	PHI0, E0[E_MAX], U0[U_MAX], M0;
};
# define u0	proj->U0
# define e0	proj->E0
# define m0	proj->M0
# define EPS10	1e-10
# define C1	.16666666666666666666
# define C2	.00833333333333333333
# define C3	.04166666666666666666
# define C4	.33333333333333333333
# define C5	.06666666666666666666
	static double
n, t, a1, c, r, d, d2, a2, tn;
FORWARD(e_forward); /* ellipsoid */
	n = sin(phi); n = 1./sqrt(1. - es * n * n);
	tn = tan(phi); t = tn * tn;
	c = cos(phi);
	a1 = lam * c;
	c *= es * c / (1 - es);
	a2 = a1 * a1;
	x = n * a1 * (1. - a2 * t * (C1 - (8. - t + 8. * c) * a2 * C2));
	y = mlfn_(e0, phi) - m0 +
		n * tn * a2 * (.5 + (5. - t + 6. * c) * a2 * C3);
	return (&xy);
}
INVERSE(e_inverse); /* ellipsoid */
	double ph1;

	ph1 = mufn_(u0, (m0 + y) / e0[0]);
	tn = tan(ph1); t = tn * tn;
	n = sin(ph1);
	r = 1. / (1. - es * n * n);
	n = sqrt(r);
	r *= (1. - es) * n;
	d = x / n;
	d2 = d * d;
	phi = ph1 - (n * tn / r) * d2 * (.5 - (1. + 3. * t) * d2 * C3);
	lam = d * (1. + t * d2 * (-C4 + (1. + 3. * t) * d2 * C5)) / cos(ph1);
	return (&lp);
}
FORWARD(s_forward); /* spheroid */
	x = asin(cos(phi) * sin(lam));
	y = atan2(tan(phi) , cos(lam)) - phi0;
	return (&xy);
}
INVERSE(s_inverse); /* spheroid */
	phi = asin(sin(d = y + phi0) * cos(x));
	lam = atan2(tan(x), cos(d));
	return (&lp);
}
ENTRY(cass) {
	if (! proj) return (sizeof(struct _PROJ));
	if (comset(I_ALL, proj, param)) ERROR;
	phi0 = *(*param)("rlat_0", "0.");
	if (es) {
		proj->forward = e_forward;
		proj->inverse = e_inverse;
		enfn_(e0, es);
		mifn_(u0, es);
		m0 = mlfn_(e0, phi0);
	} else {
		proj->forward = s_forward;
		proj->inverse = s_inverse;
	}
	return (1);
}
