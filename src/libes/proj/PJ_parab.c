/*  Craster Parabolic Projection */
#ifndef lint
static char RCSID[] = "@(#)$Id: PJ_parab.c,v 4.2 1992/07/14 01:27:47 gie Exp $";
#endif
#define __PJ_LIB
# include	"projects.h"
#define XM	0.97720502380583984317
#define RXM	1.02332670794648848847
#define YM	3.06998012383946546542
#define RYM	0.32573500793527994772
#define THIRD	0.333333333333333333
FORWARD(s_forward) { XY xy;  /* spheroid */
	lp.phi *= THIRD;
	xy.x = XM * lp.lam * (2. * cos(lp.phi + lp.phi) - 1.);
	xy.y = YM * sin(lp.phi);
	return (xy);
}
INVERSE(s_inverse) { LP lp;  /* spheroid */
	lp.phi = 3. * asin(xy.y * RYM);
	lp.lam = xy.x * RXM / (2. * cos((lp.phi + lp.phi) * THIRD) - 1);
	return (lp);
}
FREEUP {  if (P) free(P); }
ENTRY(pj_parab) {
	if (!P)
		return((PJ *)malloc(sizeof(PJ)));
	P->inv = s_inverse; P->fwd = s_forward; P->pfree = freeup;
	return P;
}
