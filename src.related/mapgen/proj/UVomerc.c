#ifndef lint
static char *SCCSID = "@(#)UVomerc.c	USGS v.3.2";
#endif
/* Oblique Mercator */
# include	"projects.h"
	static double
ks0, alpha, lamc, lam1, phi1, lam2, phi2,
Gamma, al, bl, el, singam, cosgam, sinalf, cosalf;
	static int
ellips, rot;
# define TOL	1.e-7
# define EPS	1.e-10
# define TSFN0(x)	tan(.5 * (HALFPI - (x)))
#ifdef FOR_CODE
FORWARD(e_forward); /* ellipsoid & spheroid */
	double  con, q, s, sinph0, ul, us, vl, vs;

	vl = sin(bl * lam);
	if (fabs(fabs(phi) - HALFPI) <= EPS) {
		ul = phi < 0. ? -singam : singam;
		us = al * phi / bl;
	} else {
		q = el / (ellips ? pow(tsfn_(phi, sin(phi)), bl) :
		   TSFN0(phi));
		s = .5 * (q - 1. / q);
		ul = 2. * (s * singam - vl * cosgam) / (q + 1. / q);
		con = cos(bl * lam);
		if (fabs(con) >= TOL) {
			us = al * atan((s * cosgam + vl * singam) / con) / bl;
			if (con < 0.)
				us = us + PI * al / bl;
		} else
			us = al * bl * lam;
	}
	if (fabs(fabs(ul) - 1.) <= EPS) F_ERROR;
	vs = .5 * al * log((1. - ul) / (1. + ul)) / bl;
	if (! rot) {
		x = us;
		y = vs;
	} else {
		x = vs * cosalf + us * sinalf;
		y = us * cosalf - vs * sinalf;
	}
	return (xy);
}
#else
NULL_FORWARD(e_forward);
#endif
#ifdef INV_CODE
INVERSE(e_inverse); /* ellipsoid & spheroid */
	double  q, s, ul, us, vl, vs;

	if (! rot) {
		us = x;
		vs = y;
	} else {
		vs = x * cosalf - y * sinalf;
		us = y * cosalf + x * sinalf;
	}
	q = exp(- bl * vs / al);
	s = .5 * (q - 1. / q);
	vl = sin(bl * us / al);
	ul = 2. * (vl * cosgam + s * singam) / (q + 1. / q);
	if (fabs(fabs(ul) - 1.) < EPS) {
		lam = 0.;
		phi = ul < 0. ? -HALFPI : HALFPI;
	} else {
		phi = el / sqrt((1. + ul) / (1. - ul));
		if (ellips) {
			if ((phi = phi2_(pow(phi, 1. / bl))) == HUGE)
				I_ERROR;
		} else
			phi = HALFPI - 2. * atan(phi);
		lam = - atan2((s * cosgam -
			vl * singam), cos(bl * us / al)) / bl;
	}
	return (lp);
}
#else
NULL_INVERSE(e_inverse);
#endif
ENTRY(omerc) {
	double con, com, cosph0, d, f, h, l, sinph0, p, j;
	int azi;

	lam1	= *(*param)("rlon_1",	"");
	phi1	= *(*param)("rlat_1",	"");
	lam2	= *(*param)("rlon_2",	"");
	phi2	= *(*param)("rlat_2",	"");
	lamc	= *(*param)("rlonc",	"");
	ks0	= *(*param)("dk",	"1.");
	alpha	= *(*param)("ralpha",	"");
	azi	= *(int *)(*param)("balpha", "");
	rot	= *(int *)(*param)("bno_rot", "") == 0;
	if ( (azi && (
		fabs(phi0) <= TOL ||
		fabs(fabs(phi0) - HALFPI) <= TOL ||
		fabs(fabs(alpha) - HALFPI) <= TOL
	   )) || (!azi && (
		fabs(phi1 - phi2) <= TOL ||
		(con = fabs(phi1)) <= TOL ||
		fabs(con - HALFPI) <= TOL ||
		fabs(fabs(phi0) - HALFPI) <= TOL ||
		fabs(fabs(phi2) - HALFPI) <= TOL
	   )) ) {
		emess(-1,"specifications error");
		E_ERROR;
	}
	com = (ellips = es > 0.) ? sqrt(one_es) : 1.;
	if (fabs(phi0) > EPS) {
		sinph0 = sin(phi0);
		cosph0 = cos(phi0);
		if (ellips) {
			con = 1. - es * sinph0 * sinph0;
			bl = cosph0 * cosph0;
			bl = sqrt(1. + es * bl * bl / one_es);
			al = bl * ks0 * com / con;
			d = bl * com / (cosph0 * sqrt(con));
		} else {
			bl = 1.;
			al = ks0;
			d = 1. / cosph0;
		}
		if ((f = d * d - 1.) <= 0.)
			f = 0.;
		else {
			f = sqrt(f);
			if (phi0 < 0.)
				f = -f;
		}
		el = f += d;
		if (ellips)	el *= pow(tsfn_(phi0, sinph0), bl);
		else		el *= TSFN0(phi0);
	} else {
		bl = 1. / com;
		al = ks0;
		el = d = f = 1.;
	}
	if (azi) {
		Gamma = asin(sin(alpha) / d);
		lam0 = lamc - asin((.5 * (f - 1. / f)) *
		   tan(Gamma)) / bl;
	} else {
		if (ellips) {
			h = pow(tsfn_(phi1, sin(phi1)), bl);
			l = pow(tsfn_(phi2, sin(phi2)), bl);
		} else {
			h = TSFN0(phi1);
			l = TSFN0(phi2);
		}
		f = el / h;
		p = l * h;
		j = el * el;
		j = (j - p) * (l + h) / ((j + p) * (l - h));
		if ((con = lam1 - lam2) < -PI)
			lam2 -= TWOPI;
		else if (con > PI)
			lam2 += TWOPI;
		lam0 = adjlon(.5 * (lam1 + lam2) - atan(
		   j * tan(.5 * bl * (lam1 - lam2))) / bl);
		Gamma = atan(2. * sin(bl * adjlon(lam1 - lam0)) /
		   (f - 1. / f));
		alpha = asin(d * sin(Gamma));
	}
	singam = sin(Gamma);
	cosgam = cos(Gamma);
	sinalf = sin(alpha);
	cosalf = cos(alpha);
	if (inverse) RETURN(e_inverse); else RETURN(e_forward);
}
