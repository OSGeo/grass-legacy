/* Miller Cylindrical projection */
#ifndef lint
static char RCSID[] = "@(#)$Id: PJ_mill.c,v 4.2 1992/07/14 01:27:40 gie Exp $";
#endif
#define __PJ_LIB
# include	"projects.h"
FORWARD(s_forward) { XY xy;  /* spheroid */
	xy.x = lp.lam;
	xy.y = log(tan(FORTPI + lp.phi * .4)) * 1.25;
	return (xy);
}
INVERSE(s_inverse) { LP lp;  /* spheroid */
	lp.lam = xy.x;
	lp.phi = 2.5 * (atan(exp(.8 * xy.y)) - FORTPI);
	return (lp);
}
FREEUP {  if (P) free(P); }
ENTRY(pj_mill) {
	if (!P)
		return((PJ *)malloc(sizeof(PJ)));
	P->inv = s_inverse; P->fwd = s_forward; P->pfree = freeup;
	return P;
}
