/* Mercator projection */
#ifndef lint
static char RCSID[] = "@(#)$Id: PJ_merc.c,v 4.2 1992/07/14 01:27:39 gie Exp $";
#endif
#define __PROJ_PARMS \
	double ap;
#define __PJ_LIB
#include	"projects.h"
#define EPS10 1.e-10
FORWARD(e_forward) { XY xy;  /* ellipsoid */
	if (fabs(fabs(lp.phi) - HALFPI) <= EPS10) F_ERROR;
	xy.x = P->ap * lp.lam;
	xy.y = - P->ap * log(pj_tsfn(lp.phi, sin(lp.phi), P->e));
	return (xy);
}
FORWARD(s_forward) { XY xy;  /* spheroid */
	if (fabs(fabs(lp.phi) - HALFPI) <= EPS10) F_ERROR;
	xy.x = P->ap * lp.lam;
	xy.y = P->ap * log(tan(FORTPI + .5 * lp.phi));
	return (xy);
}
INVERSE(e_inverse) { LP lp;  /* ellipsoid */
	if ((lp.phi = pj_phi2(exp(- xy.y / P->ap), P->e)) == HUGE_VAL) I_ERROR;
	lp.lam = xy.x / P->ap;
	return (lp);
}
INVERSE(s_inverse) { LP lp;  /* spheroid */
	lp.phi = HALFPI - 2. * atan(exp(-xy.y / P->ap));
	lp.lam = xy.x / P->ap;
	return (lp);
}
FREEUP {  if (P) free(P); }
ENTRY(pj_merc) {
	double phits;

	if (!P)
		return((PJ *)malloc(sizeof(PJ)));
	phits = fabs(pj_param("rlat_ts", "")->f);
	if (phits >= HALFPI) {
		emess(-1,"lat_ts >= 90d");
		E_ERROR;
	}
	if (P->es) { /* ellipsoid */
		P->ap = pj_msfn(sin(phits), cos(phits), P->es);
		P->inv = e_inverse;
		P->fwd = e_forward;
	} else { /* sphere */
		P->ap = cos(phits);
		P->inv = s_inverse;
		P->fwd = s_forward;
	}
	P->pfree = freeup;
	return P;
}
