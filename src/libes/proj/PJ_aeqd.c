/* Azimuthal Equidistant projection */
#ifndef lint
static char RCSID[] = "@(#)$Id: PJ_aeqd.c,v 4.3 1992/07/14 01:27:03 gie Exp $";
#endif
#define EPS10 1.e-10
#define __PROJ_PARMS \
	double	sinph0; \
	double	cosph0; \
	int		mode;
#define __PJ_LIB
#include	"projects.h"
#define N_POLE	0
#define S_POLE 1
#define EQUIT	2
#define OBLIQ	3
FORWARD(s_forward) { XY xy;  /* spherical */
	double  coslam, cosphi, sinphi;

	sinphi = sin(lp.phi);
	cosphi = cos(lp.phi);
	coslam = cos(lp.lam);
	switch (P->mode) {
	case EQUIT:
		xy.y = cosphi * coslam;
		goto oblcon;
	case OBLIQ:
		xy.y = P->sinph0 * sinphi + P->cosph0 * cosphi * coslam;
oblcon:
		if (fabs(fabs(xy.y) - 1.) < EPS10) {
			if (xy.y < 0.) F_ERROR;
			xy.y = 1.;
		} else {
			xy.y = acos(xy.y);
			xy.y /= sin(xy.y);
		}
		xy.x = xy.y * cosphi * sin(lp.lam);
		xy.y *= (P->mode == EQUIT) ? sinphi :
		   P->cosph0 * sinphi - P->sinph0 * cosphi * coslam;
		break;
	case N_POLE:
		lp.phi = -lp.phi;
		coslam = -coslam;
	case S_POLE:
		if (fabs(lp.phi - HALFPI) < EPS10) F_ERROR;
		xy.x = (xy.y = (HALFPI + lp.phi)) * sin(lp.lam);
		xy.y *= coslam;
		break;
	}
	return (xy);
}
INVERSE(s_inverse) { LP lp;  /* spherical */
	double cosc, rh, sinc, c;

	if ((c = (rh = hypot(xy.x, xy.y))) > PI) I_ERROR;
	switch (P->mode) {
	case OBLIQ:
	case EQUIT:
		if (fabs(rh) <= EPS10)
			lp.phi = P->phi0;
		else {
			sinc = sin(c);
			cosc = cos(c);
			if (P->mode == EQUIT) {
				lp.phi = xy.y * sinc / rh;
				if (fabs(lp.phi) >= 1.)
					lp.phi = lp.phi > 0. ? HALFPI : - HALFPI;
				else
					lp.phi = asin(lp.phi);
				xy.x *= sinc;
				xy.y = cosc * rh;
			} else {
				lp.phi = cosc * P->sinph0 + xy.y * sinc * P->cosph0 / rh;
				if (fabs(lp.phi) >= 1.)
					lp.phi = lp.phi > 0. ? HALFPI : - HALFPI;
				else
					lp.phi = asin(lp.phi);
				xy.y = (cosc - P->sinph0 * sin(lp.phi)) * rh;
				xy.x *= sinc * P->cosph0;
			}
		}
		break;
	case N_POLE:
		xy.y = -xy.y;
		lp.phi = HALFPI - c;
		break;
	case S_POLE:
		lp.phi = c - HALFPI;
		break;
	}
	lp.lam = (xy.y == 0. && (P->mode == EQUIT || P->mode == OBLIQ)) ?
		0. : atan2(xy.x, xy.y);
	return (lp);
}
FREEUP {  if (P) free(P); }
ENTRY(pj_aeqd) {
	if (!P)
		P = (PJ *)malloc(sizeof(PJ));
	else {
		P->phi0 = pj_param("rlat_0", "")->f;
		if (fabs(fabs(P->phi0) - HALFPI) < EPS10)
			P->mode = P->phi0 < 0. ? S_POLE : N_POLE;
		else if (fabs(P->phi0) < EPS10)
			P->mode = EQUIT;
		else {
			P->mode = OBLIQ;
			P->sinph0 = sin(P->phi0);
			P->cosph0 = cos(P->phi0);
		}
		P->inv = s_inverse; P->fwd = s_forward;
		P->pfree = freeup;
	}
	return P;
}
