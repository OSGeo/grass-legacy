/*  Oblique Cylindrical Equal Area */
#ifndef lint
static char RCSID[] = "@(#)$Id: PJ_ocea.c,v 4.3 1992/07/14 01:27:44 gie Exp $";
#endif
#define __PROJ_PARMS \
	double	rok; \
	double	rtk; \
	double	sinphi; \
	double	cosphi; \
	double	singam; \
	double	cosgam;
#define __PJ_LIB
#include	"projects.h"
FORWARD(s_forward) { XY xy;  /* spheroid */
	double t;

	xy.y = sin(lp.lam);
/*
	xy.x = atan2((tan(lp.phi) * P->cosphi + P->sinphi * xy.y) , cos(lp.lam));
*/
	t = cos(lp.lam);
	xy.x = atan((tan(lp.phi) * P->cosphi + P->sinphi * xy.y) / t);
	if (t < 0.)
		xy.x += PI;
	xy.x *= P->rtk;
	xy.y = P->rok * (P->sinphi * sin(lp.phi) - P->cosphi * cos(lp.phi) * xy.y);
	return (xy);
}
INVERSE(s_inverse) { LP lp;  /* spheroid */
	double t, s;

	xy.y /= P->rok;
	xy.x /= P->rtk;
	t = sqrt(1. - xy.y * xy.y);
	lp.phi = asin(xy.y * P->sinphi + t * P->cosphi * (s = sin(xy.x)));
	lp.lam = atan2(t * P->sinphi * s - xy.y * P->cosphi,
		t * cos(xy.x));
	return (lp);
}
FREEUP {  if (P) free(P); }
ENTRY(pj_ocea) {
	double k_0, phi_0, phi_1, phi_2, lam_1, lam_2, lonz, alpha;

	if (!P)
		return((PJ *)malloc(sizeof(PJ)));
	if ((k_0 = pj_param("dk", "1.")->f) <= 0.) {
		emess(-1,"k <= 0");
		E_ERROR;
	}
	P->rok = P->a / k_0;
	P->rtk = P->a * k_0;
	phi_0 = pj_param("rlat_0","")->f;
	phi_1 = pj_param("rlat_1","")->f;
	phi_2 = pj_param("rlat_2","")->f;
	lam_1 = pj_param("rlon_1","")->f;
	lam_2 = pj_param("rlon_2","")->f;
	lonz = pj_param("rlonc","")->f;
	alpha	= pj_param("ralpha",	"")->f;
	if ( pj_param("bazi","F")->i ) {
		P->singam = atan(-cos(alpha)/(-sin(phi_0) * sin(alpha))) + lonz;
		P->sinphi = asin(cos(phi_0) * sin(alpha));
	} else {
		P->singam = atan2(cos(phi_1) * sin(phi_2) * cos(lam_1) -
			sin(phi_1) * cos(phi_2) * cos(lam_2),
			sin(phi_1) * cos(phi_2) * sin(lam_2) -
			cos(phi_1) * sin(phi_2) * sin(lam_1) );
		P->sinphi = atan(-cos(P->singam - lam_1) / tan(phi_1));
	}
	P->lam0 = P->singam + HALFPI;
	P->cosphi = cos(P->sinphi);
	P->sinphi = sin(P->sinphi);
	P->cosgam = cos(P->singam);
	P->singam = sin(P->singam);
	P->inv = s_inverse;
	P->fwd = s_forward;
	P->pfree = freeup;
	return P;
}
