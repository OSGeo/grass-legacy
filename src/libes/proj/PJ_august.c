/*  August Epicycloidal */
#ifndef lint
static char RCSID[] = "@(#)$Id: PJ_august.c,v 4.2 1992/07/14 01:27:07 gie Exp $";
#endif
#define __PJ_LIB
#include	"projects.h"
#define M 1.333333333333333
FORWARD(s_forward) { XY xy;  /* spheroid */
	double t, c1, c, x1, x12, y1, y12;

	t = tan(.5 * lp.phi);
	c1 = sqrt(1. - t * t);
	c = 1. + c1 * cos(lp.lam *= .5);
	x1 = sin(lp.lam) *  c1 / c;
	y1 =  t / c;
	xy.x = M * x1 * (3. + (x12 = x1 * x1) - 3. * (y12 = y1 *  y1));
	xy.y = M * y1 * (3. + 3. * x12 - y12);
	return (xy);
}
FREEUP {  if (P) free(P); }
ENTRY(pj_august) {
	if (!P)
		return((PJ *)malloc(sizeof(PJ)));
	P->inv = 0;
	P->fwd = s_forward;
	P->pfree = freeup;
	return P;
}
