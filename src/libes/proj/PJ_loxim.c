/*  Loximuthal Projection */
#ifndef lint
static char RCSID[] = "@(#)$Id: PJ_loxim.c,v 4.2 1992/07/14 01:27:34 gie Exp $";
#endif
#define __PROJ_PARMS \
	double phi1; \
	double cosphi1; \
	double tanphi1;
#define __PJ_LIB
#include	"projects.h"
#define EPS	1e-8
FORWARD(s_forward) { XY xy;  /* spheroid */
	xy.y = lp.phi - P->phi1;
	if (fabs(xy.y) < EPS)
		xy.x = lp.lam * P->cosphi1;
	else {
		xy.x = FORTPI + 0.5 * lp.phi;
		if (fabs(xy.x) < EPS || fabs(fabs(xy.x) - HALFPI) < EPS)
			xy.x = 0.;
		else
			xy.x = lp.lam * xy.y / log( tan(xy.x) / P->tanphi1 );
	}
	return (xy);
}
INVERSE(s_inverse) { LP lp;  /* spheroid */
	lp.phi = xy.y + P->phi1;
	if (fabs(xy.y) < EPS)
		lp.lam = xy.x / P->cosphi1;
	else
		if (fabs( lp.lam = FORTPI + 0.5 * lp.phi ) < EPS ||
			fabs(fabs(lp.lam) - HALFPI) < EPS)
			lp.lam = 0.;
		else
			lp.lam = xy.x * log( tan(lp.lam) / P->tanphi1 ) / xy.y ;
	return (lp);
}
FREEUP {  if (P) free(P); }
ENTRY(pj_loxim) {
	if (!P)
		return((PJ *)malloc(sizeof(PJ)));
	P->phi1 = pj_param("rlat_1", "0")->f;
	if ((P->cosphi1 = cos(P->phi1)) < EPS) {
		emess(-1,"| lat_1 | ~= 90");
		E_ERROR;
	}
	P->tanphi1 = tan(FORTPI + 0.5 * P->phi1);
	P->inv = s_inverse; P->fwd = s_forward; P->pfree = freeup;
	return P;
}
