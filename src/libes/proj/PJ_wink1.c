/*  Winkel I */
#ifndef lint
static char RCSID[] = "@(#)$Id: PJ_wink1.c,v 4.2 1992/07/14 01:28:55 gie Exp $";
#endif
#define __PROJ_PARMS \
	double	cosphi1;
#define __PJ_LIB
# include	"projects.h"
FORWARD(s_forward) { XY xy;  /* spheroid */
	xy.x = .5 * lp.lam * (P->cosphi1 + cos(lp.phi));
	xy.y = lp.phi;
	return (xy);
}
INVERSE(s_inverse) { LP lp;  /* spheroid */
	lp.phi = xy.y;
	lp.lam = 2. * xy.x / (P->cosphi1 + cos(lp.phi));
	return (lp);
}
FREEUP {  if (P) free(P); }
ENTRY(pj_wink1) {
	if (!P)
		return((PJ *)malloc(sizeof(PJ)));
	P->cosphi1 = cos(pj_param("rlat_ts","")->f);
	P->inv = s_inverse; P->fwd = s_forward; P->pfree = freeup;
	return P;
}
