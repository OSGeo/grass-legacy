/*  Transverse Cylindrical Equal Area */
#ifndef lint
static char RCSID[] = "@(#)$Id: PJ_tcea.c,v 4.2 1992/07/14 01:28:48 gie Exp $";
#endif
#define __PROJ_PARMS \
	double rok; \
	double rtk;
#define __PJ_LIB
#include	"projects.h"
FORWARD(s_forward) { XY xy;  /* spheroid */
	xy.x = P->rok * cos(lp.phi) * sin(lp.lam);
	xy.y = P->rtk * (atan2(tan(lp.phi), cos(lp.lam)) - P->phi0);
	return (xy);
}
INVERSE(s_inverse) { LP lp;  /* spheroid */
	double t;

	xy.y = xy.y / P->rtk + P->phi0;
	xy.x /= P->rok;
	t = sqrt(1. - xy.x * xy.x);
	lp.phi = asin(t * sin(xy.y));
	lp.lam = atan2(xy.x, t * cos(xy.y));
	return (lp);
}
FREEUP {  if (P) free(P); }
ENTRY(pj_tcea) {
	double k_0;

	if (!P)
		return((PJ *)malloc(sizeof(PJ)));
	if (!(k_0 = pj_param("dk", "1.")->f)) E_ERROR;
	P->rok = P->a / k_0;
	P->rtk = P->a * k_0;
	P->inv = s_inverse;
	P->fwd = s_forward;
	P->pfree = freeup;
	return P;
}
