/*  Eckert V */
#ifndef lint
static char RCSID[] = "@(#)$Id: PJ_eck5.c,v 4.2 1992/07/14 01:27:21 gie Exp $";
#endif
#define __PJ_LIB
# include	"projects.h"
#define XF	0.44101277172455148219
#define RXF	2.26750802723822639137
#define YF	0.88202554344910296438
#define RYF	1.13375401361911319568
FORWARD(s_forward) { XY xy;  /* spheroid */
	xy.x = XF * (1. + cos(lp.phi)) * lp.lam;
	xy.y = YF * lp.phi;
	return (xy);
}
INVERSE(s_inverse) { LP lp;  /* spheroid */
	lp.lam = RXF * xy.x / (1. + cos( lp.phi = RYF * xy.y));
	return (lp);
}
FREEUP {  if (P) free(P); }
ENTRY(pj_eck5) {
	if (!P)
		return((PJ *)malloc(sizeof(PJ)));
	P->inv = s_inverse;
	P->fwd = s_forward;
	P->pfree = freeup;
	return P;
}
