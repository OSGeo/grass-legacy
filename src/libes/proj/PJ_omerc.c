/* Oblique Mercator */
#ifndef lint
static char RCSID[] = "@(#)$Id: PJ_omerc.c,v 4.4 1992/07/14 01:27:45 gie Exp $";
#endif
#define __PROJ_PARMS \
	double	ks0; \
	double	alpha; \
	double	lamc; \
	double	lam1; \
	double	phi1; \
	double	lam2; \
	double	phi2; \
	double	Gamma; \
	double	al; \
	double	bl; \
	double	el; \
	double	singam; \
	double	cosgam; \
	double	sinalf; \
	double	cosalf; \
	int		ellips; \
	int		rot;
#define __PJ_LIB
#include	"projects.h"
#define TOL	1.e-7
#define EPS	1.e-10
#define TSFN0(x)	tan(.5 * (HALFPI - (x)))
FORWARD(e_forward) { XY xy;  /* ellipsoid & spheroid */
	double  con, q, s, ul, us, vl, vs;

	vl = sin(P->bl * lp.lam);
	if (fabs(fabs(lp.phi) - HALFPI) <= EPS) {
		ul = lp.phi < 0. ? -P->singam : P->singam;
		us = P->al * lp.phi / P->bl;
	} else {
		q = P->el / (P->ellips ? pow(pj_tsfn(lp.phi, sin(lp.phi), P->e), P->bl) :
		   TSFN0(lp.phi));
		s = .5 * (q - 1. / q);
		ul = 2. * (s * P->singam - vl * P->cosgam) / (q + 1. / q);
		con = cos(P->bl * lp.lam);
		if (fabs(con) >= TOL) {
			us = P->al * atan((s * P->cosgam + vl * P->singam) / con) / P->bl;
			if (con < 0.)
				us = us + PI * P->al / P->bl;
		} else
			us = P->al * P->bl * lp.lam;
	}
	if (fabs(fabs(ul) - 1.) <= EPS) F_ERROR;
	vs = .5 * P->al * log((1. - ul) / (1. + ul)) / P->bl;
	if (! P->rot) {
		xy.x = us;
		xy.y = vs;
	} else {
		xy.x = vs * P->cosalf + us * P->sinalf;
		xy.y = us * P->cosalf - vs * P->sinalf;
	}
	return (xy);
}
INVERSE(e_inverse) { LP lp;  /* ellipsoid & spheroid */
	double  q, s, ul, us, vl, vs;

	if (! P->rot) {
		us = xy.x;
		vs = xy.y;
	} else {
		vs = xy.x * P->cosalf - xy.y * P->sinalf;
		us = xy.y * P->cosalf + xy.x * P->sinalf;
	}
	q = exp(- P->bl * vs / P->al);
	s = .5 * (q - 1. / q);
	vl = sin(P->bl * us / P->al);
	ul = 2. * (vl * P->cosgam + s * P->singam) / (q + 1. / q);
	if (fabs(fabs(ul) - 1.) < EPS) {
		lp.lam = 0.;
		lp.phi = ul < 0. ? -HALFPI : HALFPI;
	} else {
		lp.phi = P->el / sqrt((1. + ul) / (1. - ul));
		if (P->ellips) {
			if ((lp.phi = pj_phi2(pow(lp.phi, 1. / P->bl), P->e)) == HUGE_VAL)
				I_ERROR;
		} else
			lp.phi = HALFPI - 2. * atan(lp.phi);
		lp.lam = - atan2((s * P->cosgam -
			vl * P->singam), cos(P->bl * us / P->al)) / P->bl;
	}
	return (lp);
}
FREEUP {  if (P) free(P); }
ENTRY(pj_omerc) {
	double con, com, cosph0, d, f, h, l, sinph0, p, j;
	int azi;

	if (!P)
		return((PJ *)malloc(sizeof(PJ)));
	P->lam1	= pj_param("rlon_1",	"")->f;
	P->phi1	= pj_param("rlat_1",	"")->f;
	P->lam2	= pj_param("rlon_2",	"")->f;
	P->phi2	= pj_param("rlat_2",	"")->f;
	P->lamc	= pj_param("rlonc",	"")->f;
	P->ks0	= pj_param("dk",	"1.")->f;
	P->alpha	= pj_param("ralpha",	"")->f;
	azi	= pj_param("balpha", "F")->f;
	P->rot	= pj_param("bno_rot", "F")->f == 0;
	if ( (azi && (
		fabs(P->phi0) <= TOL ||
		fabs(fabs(P->phi0) - HALFPI) <= TOL ||
		fabs(fabs(P->alpha) - HALFPI) <= TOL
	   )) || (!azi && (
		fabs(P->phi1 - P->phi2) <= TOL ||
		(con = fabs(P->phi1)) <= TOL ||
		fabs(con - HALFPI) <= TOL ||
		fabs(fabs(P->phi0) - HALFPI) <= TOL ||
		fabs(fabs(P->phi2) - HALFPI) <= TOL
	   )) ) {
		emess(-1,"specifications error");
		E_ERROR;
	}
	com = (P->ellips = P->es > 0.) ? sqrt(P->one_es) : 1.;
	if (fabs(P->phi0) > EPS) {
		sinph0 = sin(P->phi0);
		cosph0 = cos(P->phi0);
		if (P->ellips) {
			con = 1. - P->es * sinph0 * sinph0;
			P->bl = cosph0 * cosph0;
			P->bl = sqrt(1. + P->es * P->bl * P->bl / P->one_es);
			P->al = P->bl * P->ks0 * com / con;
			d = P->bl * com / (cosph0 * sqrt(con));
		} else {
			P->bl = 1.;
			P->al = P->ks0;
			d = 1. / cosph0;
		}
		if ((f = d * d - 1.) <= 0.)
			f = 0.;
		else {
			f = sqrt(f);
			if (P->phi0 < 0.)
				f = -f;
		}
		P->el = f += d;
		if (P->ellips)	P->el *= pow(pj_tsfn(P->phi0, sinph0, P->e), P->bl);
		else		P->el *= TSFN0(P->phi0);
	} else {
		P->bl = 1. / com;
		P->al = P->ks0;
		P->el = d = f = 1.;
	}
	if (azi) {
		P->Gamma = asin(sin(P->alpha) / d);
		P->lam0 = P->lamc - asin((.5 * (f - 1. / f)) *
		   tan(P->Gamma)) / P->bl;
	} else {
		if (P->ellips) {
			h = pow(pj_tsfn(P->phi1, sin(P->phi1), P->e), P->bl);
			l = pow(pj_tsfn(P->phi2, sin(P->phi2), P->e), P->bl);
		} else {
			h = TSFN0(P->phi1);
			l = TSFN0(P->phi2);
		}
		f = P->el / h;
		p = l * h;
		j = P->el * P->el;
		j = (j - p) * (l + h) / ((j + p) * (l - h));
		if ((con = P->lam1 - P->lam2) < -PI)
			P->lam2 -= TWOPI;
		else if (con > PI)
			P->lam2 += TWOPI;
		P->lam0 = adjlon(.5 * (P->lam1 + P->lam2) - atan(
		   j * tan(.5 * P->bl * (P->lam1 - P->lam2))) / P->bl);
		P->Gamma = atan(2. * sin(P->bl * adjlon(P->lam1 - P->lam0)) /
		   (f - 1. / f));
		P->alpha = asin(d * sin(P->Gamma));
	}
	P->singam = sin(P->Gamma);
	P->cosgam = cos(P->Gamma);
	P->sinalf = sin(P->alpha);
	P->cosalf = cos(P->alpha);
	P->inv = e_inverse;
	P->fwd = e_forward;
	P->pfree = freeup;
	return P;
}
