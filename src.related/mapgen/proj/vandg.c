static char *SCCSID = "@(#)vandg.c	AMG v.1.1";
/* van der Grinten projection */
# define	PROJ_MOD
# include	"projects.h"
struct _PROJ { PHEAD; };
# define TOL		1.e-10
# define THIRD		.33333333333333333333
# define TWO_THRD	.66666666666666666666
# define C2_27		.07407407407407407407
# define PI4_3		4.18879020478639098458
# define PISQ		9.86960440108935861869
# define TPISQ		19.73920880217871723738
# define HPISQ		4.93480220054467930934
FORWARD(s_forward); /* spheroid */
	double  al, al2, g, g2, p2;

	p2 = fabs(phi / HALFPI);
	if ((p2 - TOL) > 1.) ERROR;
	if (p2 > 1.)
		p2 = 1.;
	if (fabs(phi) <= TOL) {
		x = lam;
		y = 0.;
	} else if (fabs(lam) <= TOL || fabs(p2 - 1.) < TOL) {
		x = 0.;
		y = PI * tan(.5 * asin(p2));
		if (phi < 0.) y = -y;
	} else {
		al = .5 * fabs(PI / lam - lam / PI);
		al2 = al * al;
		g = sqrt(1. - p2 * p2);
		g = g / (p2 + g - 1.);
		g2 = g * g;
		p2 = g * (2. / p2 - 1.);
		p2 = p2 * p2;
		x = g - p2; g = p2 + al2;
		x = PI * (al * x + sqrt(al2 * x * x - g * (g2 - p2))) / g;
		if (lam < 0.) x = -x;
		y = fabs(x / PI);
		y = 1. - y * (y + 2. * al);
		if (y < -TOL) ERROR;
		y = y < 0. ? 0. : sqrt(y) * (phi < 0. ? -PI : PI);
	}
	return (&xy);
}
INVERSE(s_inverse); /* spheroid */
	double t, c0, c1, c2, c3, al, r2, r, m, d, ay, x2, y2;

	x2 = x * x;
	if ((ay = fabs(y)) < TOL) {
		phi = 0.;
		t = x2 * x2 + TPISQ * (x2 + HPISQ);
		lam = fabs(x) <= TOL ? 0. :
		   .5 * (x2 - PISQ + sqrt(t)) / x;
		return (&lp);
	}
	y2 = y * y;
	r = x2 + y2;	r2 = r * r;
	c1 = - PI * ay * (r + PISQ);
	c3 = r2 + TWOPI * (ay * r + PI * (y2 + PI * (ay + HALFPI)));
	c2 = c1 + PISQ * (r - 3. *  y2);
	c0 = PI * ay;
	c2 /= c3;
	al = c1 / c3 - THIRD * c2 * c2;
	m = 2. * sqrt(-THIRD * al);
	d = C2_27 * c2 * c2 * c2 + (c0 * c0 - THIRD * c2 * c1) / c3;
	if (((t = fabs(d = 3. * d / (al * m))) - TOL) <= 1.) {
		d = t > 1. ? (d > 0. ? 0. : PI) : acos(d);
		phi = PI * (m * cos(d * THIRD + PI4_3) - THIRD * c2);
		if (y < 0.) phi = -phi;
		t = r2 + TPISQ * (x2 - y2 + HPISQ);
		lam = fabs(x) <= TOL ? 0. :
		   .5 * (r - PISQ + (t <= 0. ? 0. : sqrt(t))) / x;
	} else
		ERROR;
	return (&lp);
}
ENTRY(vandg) {
	if (! proj) return (sizeof(struct _PROJ));
	if (comset(I_LAM0 + I_XY, proj, param)) ERROR;
	proj->forward = s_forward;
	proj->inverse = s_inverse;
	return (1);
}
