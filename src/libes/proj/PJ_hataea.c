/*  Hatano Asymmetrical Equal Area */
#ifndef lint
static char RCSID[] = "@(#)$Id: PJ_hataea.c,v 4.2 1992/07/14 01:27:30 gie Exp $";
#endif
#define __PJ_LIB
#include	"projects.h"
#define NITER	20
#define EPS	1e-7
#define ONETOL 1.000001
#define CN	2.67595
#define CS	2.43763
#define RCN	0.37369906014686373063
#define RCS	0.41023453108141924738
#define FYCN	1.75859
#define FYCS	1.93052
#define RYCN	0.56863737426006061674
#define RYCS	0.51799515156538134803
#define FXC	0.85
#define RXC	1.17647058823529411764
FORWARD(s_forward) { XY xy;  /* spheroid */
	double th1, c;
	int i;

	c = sin(lp.phi) * (lp.phi < 0. ? CS : CN);
	for (i = NITER; i; --i) {
		lp.phi -= th1 = (lp.phi + sin(lp.phi) - c) / (1. + cos(lp.phi));
		if (fabs(th1) < EPS) break;
	}
	xy.x = FXC * lp.lam * cos(lp.phi *= .5);
	xy.y = sin(lp.phi) * (lp.phi < 0. ? FYCS : FYCN);
	return (xy);
}
INVERSE(s_inverse) { LP lp;  /* spheroid */
	double th;

	th = xy.y * ( xy.y < 0. ? RYCS : RYCN);
	if (fabs(th) > 1.)
		if (fabs(th) > ONETOL)	I_ERROR
		else			th = th > 0. ? HALFPI : - HALFPI;
	else
		th = asin(th);
	lp.lam = RXC * xy.x / cos(th);
	th += th;
	lp.phi = (th + sin(th)) * (xy.y < 0. ? RCS : RCN);
	if (fabs(lp.phi) > 1.)
		if (fabs(lp.phi) > ONETOL)	I_ERROR
		else			lp.phi = lp.phi > 0. ? HALFPI : - HALFPI;
	else
		lp.phi = asin(lp.phi);
	return (lp);
}
FREEUP {  if (P) free(P); }
ENTRY(pj_hataea) {
	if (!P)
		return((PJ *)malloc(sizeof(PJ)));
	P->inv = s_inverse; P->fwd = s_forward; P->pfree = freeup;
	return P;
}
