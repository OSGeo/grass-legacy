/* Near-sided perspective */
#ifndef lint
static char RCSID[] = "@(#)$Id: PJ_nsper.c,v 4.2 1992/07/14 01:27:43 gie Exp $";
#endif
#define __PROJ_PARMS \
	double	height; \
	double	sinph0; \
	double	cosph0; \
	double	p; \
	double	rp; \
	double	pn1; \
	double	pfact; \
	double	h; \
	double	cg; \
	double	sg; \
	double	sw; \
	double	cw; \
	int		mode; \
	int		tilt;
#define __PJ_LIB
#include	"projects.h"
# define EPS10 1.e-10
# define N_POLE	0
# define S_POLE 1
# define EQUIT	2
# define OBLIQ	3
FORWARD(s_forward) { XY xy;  /* spheroid */
	double  coslam, cosphi, sinphi;

	sinphi = sin(lp.phi);
	cosphi = cos(lp.phi);
	coslam = cos(lp.lam);
	switch (P->mode) {
	case OBLIQ:
		xy.y = P->sinph0 * sinphi + P->cosph0 * cosphi * coslam;
		break;
	case EQUIT:
		xy.y = cosphi * coslam;
		break;
	case S_POLE:
		xy.y = - sinphi;
		break;
	case N_POLE:
		xy.y = sinphi;
		break;
	}
	if (xy.y < P->rp) F_ERROR;
	xy.y = P->pn1 / (P->p - xy.y);
	xy.x = xy.y * cosphi * sin(lp.lam);
	switch (P->mode) {
	case OBLIQ:
		xy.y *= (P->cosph0 * sinphi -
		   P->sinph0 * cosphi * coslam);
		break;
	case EQUIT:
		xy.y *= sinphi;
		break;
	case N_POLE:
		coslam = - coslam;
	case S_POLE:
		xy.y *= cosphi * coslam;
		break;
	}
	if (P->tilt) {
		double yt, ba;

		yt = xy.y * P->cg + xy.x * P->sg;
		ba = 1. / (yt * P->sw * P->h + P->cw);
		xy.x = (xy.x * P->cg - xy.y * P->sg) * P->cw * ba;
		xy.y = yt * ba;
	}
	return (xy);
}
INVERSE(s_inverse) { LP lp;  /* spheroid */
	double  rh, cosz, sinz;

	if (P->tilt) {
		double bm, bq, yt;

		yt = 1./(P->pn1 - xy.y * P->sw);
		bm = P->pn1 * xy.x * yt;
		bq = P->pn1 * xy.y * P->cw * yt;
		xy.x = bm * P->cg + bq * P->sg;
		xy.y = bq * P->cg - bm * P->sg;
	}
	rh = hypot(xy.x, xy.y);
	if ((sinz = 1. - rh * rh * P->pfact) < 0.) I_ERROR;
	sinz = (P->p - sqrt(sinz)) / (P->pn1 / rh + rh / P->pn1);
	cosz = sqrt(1. - sinz * sinz);
	if (fabs(rh) <= EPS10) {
		lp.lam = 0.;
		lp.phi = P->phi0;
	} else {
		switch (P->mode) {
		case OBLIQ:
			lp.phi = asin(cosz * P->sinph0 + xy.y * sinz * P->cosph0 / rh);
			xy.y = (cosz - P->sinph0 * sin(lp.phi)) * rh;
			xy.x *= sinz * P->cosph0;
			break;
		case EQUIT:
			lp.phi = asin(xy.y * sinz / rh);
			xy.y = cosz * rh;
			xy.x *= sinz;
			break;
		case N_POLE:
			lp.phi = asin(cosz);
			xy.y = -xy.y;
			break;
		case S_POLE:
			lp.phi = - asin(cosz);
			break;
		}
		lp.lam = atan2(xy.x, xy.y);
	}
	return (lp);
}
FREEUP {  if (P) free(P); }

	static PJ *
#ifdef __STDC__
setup(PJ *P) 
#else
setup(P) 
    PJ *P;
#endif
{
	if ((P->height = pj_param("dh", "")->f) <= 0.) {
		emess(-1,"P->h <= 0.");
		E_ERROR;
	}
	if (fabs(fabs(P->phi0) - HALFPI) < EPS10)
		P->mode = P->phi0 < 0. ? S_POLE : N_POLE;
	else if (fabs(P->phi0) < EPS10)
		P->mode = EQUIT;
	else {
		P->mode = OBLIQ;
		P->sinph0 = sin(P->phi0);
		P->cosph0 = cos(P->phi0);
	}
	P->pn1 = P->height / P->a; /* normalize by radius */
	P->p = 1. + P->pn1;
	P->rp = 1. / P->p;
	P->h = 1. / P->pn1;
	P->pfact = (P->p + 1.) * P->h;
	P->inv = s_inverse;
	P->fwd = s_forward;
	P->pfree = freeup;
	return P;
}
ENTRY(pj_nsper) {
	if (!P)
		return((PJ *)malloc(sizeof(PJ)));
	P->tilt = 0;
	return setup(P);
}
ENTRY(pj_tpers) {
	double omega, gamma;

	if (!P)
		return((PJ *)malloc(sizeof(PJ)));
	omega = pj_param("dtilt", "")->f * DEG_TO_RAD;
	gamma = pj_param("dazi", "")->f * DEG_TO_RAD;
	P->tilt = 1;
	P->cg = cos(gamma); P->sg = sin(gamma);
	P->cw = cos(omega); P->sw = sin(omega);
	return setup(P);
}
