/*  Gall (Stereographic) */
#ifndef lint
static char RCSID[] = "@(#)$Id: PJ_gall.c,v 4.2 1992/07/14 01:27:26 gie Exp $";
#endif
#define __PJ_LIB
#include	"projects.h"
#define YF	1.70710678118654752440
#define XF	0.70710678118654752440
#define RYF	0.58578643762690495119
#define RXF	1.41421356237309504880
FORWARD(s_forward) { XY xy;  /* spheroid */
	xy.x = XF * lp.lam;
	xy.y = YF * tan(.5 * lp.phi);
	return (xy);
}
INVERSE(s_inverse) { LP lp;  /* spheroid */
	lp.lam = RXF * xy.x;
	lp.phi = 2. * atan(xy.y * RYF);
	return (lp);
}
FREEUP {  if (P) free(P); }
ENTRY(pj_gall) {
	if (!P)
		return((PJ *)malloc(sizeof(PJ)));
	P->inv = s_inverse; P->fwd = s_forward; P->pfree = freeup;
	return P;
}
