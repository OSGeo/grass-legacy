#ifndef lint
static char *SCCSID = "@(#)UVtmerc.c	USGS v.3.3";
#endif
/* Transverse Mercator projection and
** Universal Transverse Mercator projection */
# include	"projects.h"
	static double
ks0, esp, ml0;
# define EPS10 1.e-10
# define aks0	esp
# define aks5	ml0
# define FC1 1.
# define FC2 .5
# define FC3 .16666666666666666666
# define FC4 .08333333333333333333
# define FC5 .05
# define FC6 .03333333333333333333
# define FC7 .02380952380952380952
# define FC8 .01785714285714285714
#ifdef FOR_CODE
FORWARD(e_forward); /* ellipsoid */
	double al, als, n, cosphi, sinphi, t;

	sinphi = sin(phi); cosphi = cos(phi);
	t = fabs(cosphi) > 1e-10 ? sinphi/cosphi : 0.;
	t *= t;
	al = cosphi * lam;
	als = al * al;
	al /= sqrt(1. - es * sinphi * sinphi);
	n = esp * cosphi * cosphi;
	x = ks0 * al * (FC1 +
		FC3 * als * (1. - t + n +
		FC5 * als * (5. + t * (t - 18.) + n * (14. - 58. * t)
		+ FC7 * als * (61. + t * ( t * (179. - t) - 479. ) )
		)));
	y = ks0 * (mlfn_(phi, sinphi, cosphi) - ml0 +
		sinphi * al * lam * FC2 * ( 1. +
		FC4 * als * (5. - t + n * (9. + 4. * n) +
		FC6 * als * (61. + t * (t - 58.) + n * (270. - 330 * t)
		+ FC8 * als * (1385. + t * ( t * (543. - t) - 3111.) )
		))));
	return (xy);
}
FORWARD(s_forward); /* sphere */
	double b, cosphi;

	b = (cosphi = cos(phi)) * sin(lam);
	if (fabs(fabs(b) - 1.) <= EPS10) F_ERROR;
	x = aks5 * log((1. + b) / (1. - b));
	if ((b = fabs( y = cosphi * cos(lam) / sqrt(1. - b * b) )) >= 1.) {
		if ((b - 1.) > EPS10) F_ERROR
		else y = 0.;
	} else
		y = acos(y);
	if (phi < 0.) y = -y;
	y = aks0 * (y - phi0);
	return (xy);
}
#else
NULL_FORWARD(s_forward);
NULL_FORWARD(e_forward);
#endif
#ifdef INV_CODE
INVERSE(e_inverse); /* ellipsoid */
	double n, con, cosphi, d, ds, sinphi, t;

	phi = inv_mlfn_(ml0 + y / ks0);
	if (fabs(phi) >= HALFPI) {
		phi = y < 0. ? -HALFPI : HALFPI;
		lam = 0.;
	} else {
		sinphi = sin(phi);
		cosphi = cos(phi);
		t = fabs(cosphi) > 1e-10 ? sinphi/cosphi : 0.;
		n = esp * cosphi * cosphi;
		d = x * sqrt(con = 1. - es * sinphi * sinphi) / ks0;
		con *= t;
		t *= t;
		ds = d * d;
		phi -= (con * ds / (1.-es)) * FC2 * (1. -
			ds * FC4 * (5. + t * (3. - 9. *  n) + n * (1. - 4 * n) -
			ds * FC6 * (61. + t * (90. - 252. * n +
				45. * t) + 46. * n
		   - ds * FC8 * (1385. + t * (3633. + t * (4095. + 1574. * t)) )
			)));
		lam = d*(FC1 -
			ds*FC3*( 1. + 2.*t + n -
			ds*FC5*(5. + t*(28. + 24.*t + 8.*n) + 6.*n
		   - ds * FC7 * (61. + t * (662. + t * (1320. + 720. * t)) )
		))) / cosphi;
	}
	return (lp);
}
INVERSE(s_inverse); /* sphere */
	double h, g;

	h = exp(x / aks0);
	g = .5 * (h - 1. / h);
	h = cos(phi0 + y) / aks0;
	phi = asin(sqrt((1. - h * h) / (1. + g * g)));
	if (y < 0.) phi = -phi;
	lam = (g || h) ? atan2(g, h) : 0.;
	return (lp);
}
#else
NULL_INVERSE(s_inverse);
NULL_INVERSE(e_inverse);
#endif
	static
ENTRY(setup) { /* general initialization */
	if (es) {
		enfn_();
		ml0 = mlfn_(phi0, sin(phi0), cos(phi0));
		esp = es / (1. - es);
		if (inverse) RETURN(e_inverse); else RETURN(e_forward);
	} else {
		aks0 = ks0;
		aks5 = .5 * aks0;
		if (inverse) RETURN(s_inverse); else RETURN(s_forward);
	}
}
ENTRY(tmerc) {
	ks0 = *(*param)("dk", "1.");
	return (setup(inverse));
}
ENTRY(utm) {
	int zone;

	y0 = *(int *)(*param)("bsouth") ? 10000000. : 0.;
	x0 = 500000.;
	zone = *(int *)(*param)("izone", "9999");
	if (zone != 9999) /* zone input form */
		if (zone > 0 && zone <= 60)
			--zone;
		else {
			emess(-1,"invalid zone number");
			E_ERROR;
		}
	else /* nearest central meridian input */
		if ((zone = floor((adjlon(lam0) + PI) * 30. / PI)) < 0)
			zone = 0;
		else if (zone >= 60)
			zone = 59;
	lam0 = (zone + .5) * PI / 30. - PI;
	ks0 = 0.9996;
	phi0 = 0.;
	return (setup(inverse));
}
