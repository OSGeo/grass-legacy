/* Equidistant Cylindrical projection (Plate Caree) */
#ifndef lint
static char RCSID[] = "@(#)$Id: PJ_eqc.c,v 4.2 1992/07/14 01:27:23 gie Exp $";
#endif
#define __PROJ_PARMS \
	double rc;
#define __PJ_LIB
# include	"projects.h"
FORWARD(s_forward) { XY xy;  /* spheroid */
	xy.x = P->rc * lp.lam;
	xy.y = lp.phi;
	return (xy);
}
INVERSE(s_inverse) { LP lp;  /* spheroid */
	lp.phi = xy.y;
	lp.lam = xy.x / P->rc;
	return (lp);
}
FREEUP {  if (P) free(P); }
ENTRY(pj_eqc) {
	if (!P)
		return((PJ *)malloc(sizeof(PJ)));
	if ((P->rc = cos(pj_param("rlat_ts", "")->f)) <= 0.) {
		emess(-1,"lat_ts >= 90d");
		E_ERROR;
	}
	P->inv = s_inverse;
	P->fwd = s_forward;
	P->pfree = freeup;
	return P;
}
