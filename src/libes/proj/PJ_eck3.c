/*  Eckert III */
#ifndef lint
static char RCSID[] = "@(#)$Id: PJ_eck3.c,v 4.2 1992/07/14 01:27:19 gie Exp $";
#endif
#define __PJ_LIB
#include	"projects.h"
#define XF	.42223820031577120149
#define RXF	2.36833142821314873781
#define YF	.84447640063154240298
#define RYF	1.18416571410657436890
FORWARD(s_forward) { XY xy;  /* spheroid */
	if (fabs(xy.x = lp.phi / HALFPI) >= 1.)
		xy.x = XF * lp.lam;
	else
		xy.x = XF * (1. + sqrt(1. - xy.x*xy.x)) * lp.lam;
	xy.y = YF * lp.phi;
	return (xy);
}
INVERSE(s_inverse) { LP lp;  /* spheroid */
	double t;

	lp.lam = xy.x * RXF;
	if (fabs(t = (lp.phi = RYF * xy.y) / HALFPI) < 1.)
		lp.lam /= 1. + sqrt(1. - t*t);
	return (lp);
}
FREEUP {  if (P) free(P); }
ENTRY(pj_eck3) {
	if (!P)
		return((PJ *)malloc(sizeof(PJ)));
	P->inv = s_inverse;
	P->fwd = s_forward;
	P->pfree = freeup;
	return P;
}
