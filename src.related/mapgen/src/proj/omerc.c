static char *SCCSID = "@(#)omerc.c	AMG v.1.1";
/* Oblique Mercator */
# define	PROJ_MOD
# include	"projects.h"
struct _PROJ { PHEAD;
	double	PHI0, KS0, ALPHA, LAMC, LAM1, PHI1, LAM2, PHI2,
		E, GAMMA, AL, BL, EL, SINGAM,
		COSGAM, SINALF, COSALF;
	int	ELLIPS, ROT;
};
# define TOL	1.e-7
# define EPS	1.e-10
# define phi0	proj->PHI0
# define ks0	proj->KS0
# define alpha	proj->ALPHA
# define lamc	proj->LAMC
# define lam1	proj->LAM1
# define lam2	proj->LAM2
# define rot	proj->ROT
# define ellips	proj->ELLIPS
# define gamma	proj->GAMMA
# define al	proj->AL
# define bl	proj->BL
# define el	proj->EL
# define singam	proj->SINGAM
# define cosgam	proj->COSGAM
# define sinalf	proj->SINALF
# define cosalf	proj->COSALF
# define TSFN0(x)	tan(.5 * (HALFPI - (x)))
FORWARD(forward); /* ellipsoid & spheroid */
	double  con, q, s, sinph0, ul, us, vl, vs;

	vl = sin(bl * lam);
	if (fabs(fabs(phi) - HALFPI) <= EPS) {
		ul = phi < 0. ? -singam : singam;
		us = al * phi / bl;
	} else {
		q = el / (ellips ? pow(tsfn_(e, phi, sin(phi)), bl) :
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
	if (fabs(fabs(ul) - 1.) <= EPS) ERROR;
	vs = .5 * al * log((1. - ul) / (1. + ul)) / bl;
	if (! rot) {
		x = vs;
		y = us;
	} else {
		x = vs * cosalf + us * sinalf;
		y = us * cosalf - vs * sinalf;
	}
	return (&xy);
}
INVERSE(inverse); /* ellipsoid & spheroid */
	double  q, s, ul, us, vl, vs;

	if (! rot) {
		vs = x;
		us = y;
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
			if ((phi = phi2_(e, pow(phi, 1. / bl))) == HUGE)
				ERROR;
		} else
			phi = HALFPI - 2. * atan(phi);
		lam = - atan2((s * cosgam -
			vl * singam), cos(bl * us / al)) / bl;
	}
	return (&lp);
}
ENTRY(omerc) {
	double con, com, cosph0, d, f, h, l, sinph0, p, j;
	int azi;

	if (! proj) return (sizeof(struct _PROJ));
	if (comset(I_ALL, proj, param)) ERROR;
	phi0	= *(*param)("rlat_0",	"");
	lam1	= *(*param)("rlon_1",	"");
	phi1	= *(*param)("rlat_1",	"");
	lam2	= *(*param)("rlon_2",	"");
	phi2	= *(*param)("rlat_2",	"");
	lamc	= *(*param)("rlonc",	"");
	ks0	= *(*param)("dk",	"1.");
	alpha	= *(*param)("ralpha",	"");
	azi	= *(int *)(*param)("bazi");
	rot	= *(int *)(*param)("bno_rot") != 0;
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
	   )) )
		ERROR;
	if (ellips = es > 0.) {
		e = sqrt(es);
		com = sqrt(1. - es);
	} else {
		e = 0;
		com = 1.;
	}
	if (fabs(phi0) > EPS) {
		sinph0 = sin(phi0);
		cosph0 = cos(phi0);
		if (ellips) {
			con = 1. - es * sinph0 * sinph0;
			bl = cosph0 * cosph0;
			bl = sqrt(1. + es * bl * bl / (1. - es));
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
		el = (f += d) * (ellips ? pow(tsfn_(e, phi0, sinph0), bl) :
			TSFN0(phi0));
	} else {
		bl = 1. / com;
		al = ks0;
		el = d = f = 1.;
	}
	if (azi) {
		gamma = asin(sin(alpha) / d);
		lam0 = lamc - asin((.5 * (f - 1. / f)) *
		   tan(gamma)) / bl;
	} else {
		if (ellips) {
			h = pow(tsfn_(e, phi1, sin(phi1)), bl);
			l = pow(tsfn_(e, phi2, sin(phi2)), bl);
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
		gamma = atan(2. * sin(bl * adjlon(lam1 - lam0)) /
		   (f - 1. / f));
		alpha = asin(d * sin(gamma));
	}
	singam = sin(gamma);
	cosgam = cos(gamma);
	sinalf = sin(alpha);
	cosalf = cos(alpha);
	proj->forward = forward;
	proj->inverse = inverse;
	return (1);
}
