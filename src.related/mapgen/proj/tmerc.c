static char *SCCSID = "@(#)tmerc.c	AMG v.1.1";
/* Transverse Mercator projection and
** Universal Transverse Mercator projection */
# define	PROJ_MOD
# include	"projects.h"
struct _PROJ { PHEAD;
	double	KS0, PHI0, E, E0[E_MAX], ESP, ML0;
};
# define MAXITER 6
# define EPS10 1.e-10
# define ks0	proj->KS0
# define e	proj->E
# define e0	proj->E0
# define esp	proj->ESP
# define ml0	proj->ML0
# define aks0	proj->ESP
# define aks5	proj->ML0
FORWARD(e_forward); /* ellipsoid */
	double al, als, b, c, cosphi, ml, n, sinphi, t, tq;

	sinphi = sin(phi);
	cosphi = cos(phi);
	al = cosphi * lam;
	als = al * al;
	c = esp * cosphi * cosphi;
	tq = tan(phi);
	t = tq * tq;
	n = 1. / sqrt(1. - es * sinphi * sinphi);
	ml = mlfn_(e0, phi);
	x = ks0 * n * al * (1. + als / 6. * (1. -
	   t + c + als / 20. * (5. - 18. * t + t * t
	   + 72. * c - 58. * esp)));
	y = ks0 * (ml - ml0 + n * tq * (als * (.5 +
	   als / 24. * (5. - t + 9. * c + 4. * c * c
	   + als / 30. * (61. - 58. * t + t * t +
	   600. * c - 330. * esp)))));
	return (&xy);
}
INVERSE(e_inverse); /* ellipsoid */
	int i;
	double al, als, b, c, con, cosphi, cs, d,
		dphi, ds, f, g, h, n, r, sinphi, t, tanphi, ts;

	con = ml0 + y / ks0;
	phi = con;
	for(i = MAXITER; i ; --i) {
		dphi = ((con + e0[1] * sin(2. * phi) - e0[2] *
		   sin(4. * phi)) / e0[0]) - phi;
		phi = phi + dphi;
		if (fabs(dphi) <= EPS10)
			break;
	}
	if (! i) ERROR;
	if (fabs(phi) >= HALFPI) {
		phi = y < 0. ? -HALFPI : HALFPI;
		lam = 0.;
	} else {
		sinphi = sin(phi);
		cosphi = cos(phi);
		tanphi = tan(phi);
		c = esp * cosphi * cosphi;
		cs = c * c;
		t = tanphi * tanphi;
		ts = t * t;
		n = 1. / sqrt(con = 1. - es * sinphi * sinphi);
		r = n * (1. - es) / con;
		d = x / (n * ks0);
		ds = d * d;
		phi -= (n * tanphi * ds / r) * (.5 -
		   ds / 24. * (5. + 3. * t + 10. * c
		   -4. * cs - 9. * esp - ds / 30. * (61
		   + 90. * t + 298. * c + 45. * ts
		   - 252. * esp - 3. * cs)));
		lam = (d * (1. - ds / 6.
		   * (1. + 2. * t + c - ds / 20. * (5.
		   - 2. * c + 28. * t - 3. * cs
		   + 8. * esp + 24. * ts))) / cosphi);
	}
	return (&lp);
}
FORWARD(s_forward); /* spheroid */
	double b, cosphi, t;

	t = 1. - ( b = (cosphi = cos(phi)) * sin(lam));
	if (fabs(t) <= EPS10) ERROR;
	x = aks5 * log((1. + b) / t);
	y = acos(cosphi * cos(lam) / sqrt(1. - b * b));
	if (phi < 0.) y = -y;
	y = aks0 * (y - phi0);
	return (&xy);
}
INVERSE(s_inverse); /* spheroid */
	double h, g;

	h = exp(x / aks0);
	g = .5 * (h - 1. / h);
	h = cos(phi0 + y) / aks0;
	phi = asin(sqrt((1. - h * h) / (1. + g * g)));
	if (y < 0.) phi = -phi;
	lam = (g || h) ? atan2(g, h) : 0.;
	return (&lp);
}
	static void
setup(proj) struct _PROJ *proj; { /* general initialization */
	if (es) {
		proj->forward = e_forward;
		proj->inverse = e_inverse;
		e = sqrt( es );
		enfn_(e0, es);
		ml0 = mlfn_(e0, phi0);
		esp = es / (1. - es);
	} else {
		proj->forward = s_forward;
		proj->inverse = s_inverse;
		aks0 = ks0;
		aks5 = .5 * aks0;
	}
}
ENTRY(tmerc) {
	if (! proj) return (sizeof(struct _PROJ));
	if (comset(I_ALL, proj, param)) ERROR;
	phi0 = *(*param)("rlat_0", "");
	ks0 = *(*param)("dk", "1.");
	setup(proj);
	return (1);
}
ENTRY(utm) {
	int zone;

	if (! proj) return (sizeof(struct _PROJ));
	if (comset(I_ELLPS + I_LAM0, proj, param) || es <= 0.) ERROR;
	y0 = *(int *)(*param)("bsouth") ? 10000000. : 0.;
	x0 = 500000.;
	zone = *(int *)(*param)("izone", "9999");
	if (zone != 9999) /* zone input form */
		if (zone > 0 && zone <= 60)
			--zone;
		else ERROR; /* invalid zone no. */
	else /* nearest central meridian input */
		if ((zone = floor((adjlon(lam0) + PI) * 30. / PI)) < 0)
			zone = 0;
		else if (zone >= 60)
			zone = 59;
	lam0 = (zone + .5) * PI / 30. - PI;
	ks0 = 0.9996;
	phi0 = 0.;
	setup(proj);
	return (1);
}
