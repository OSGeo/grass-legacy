/*  McBryde-Thomas Flat-Polar Sinusoidal */
#ifndef lint
static char RCSID[] = "@(#)$Id: PJ_mbtfps.c,v 4.3 1992/07/14 01:27:38 gie Exp $";
#endif
#define __PJ_LIB
#include	"projects.h"
#define NITER	20
#define EPS	1e-7
#define ONETOL 1.000001
#define C	1.78539816339744830961
#define RC	0.56009915351155737591
#define FYC	0.91659627441275150748
#define RYC	1.09099286994231339007
#define FXC	0.61106418294183433832
#define RXC	1.63648930491347008510
FORWARD(s_forward) { XY xy;  /* spheroid */
	double th1, c;
	int i;

	c = C * sin(lp.phi);
	for (i = NITER; i; --i) {
		lp.phi -= th1 = (0.5 * lp.phi + sin(lp.phi) - c )/(0.5 + cos(lp.phi));
		if (fabs(th1) < EPS) break;
	}
	xy.x = FXC * lp.lam * (0.5 + cos(lp.phi));
	xy.y = FYC * lp.phi;
	return (xy);
}
INVERSE(s_inverse) { LP lp;  /* spheroid */
	lp.phi = RYC * xy.y;
	lp.lam = RXC * xy.x / (0.5 + cos(lp.phi));
	lp.phi = RC * (0.5 * lp.phi + sin(lp.phi));
	if (fabs(lp.phi) > 1.)
		if (fabs(lp.phi) > ONETOL)	I_ERROR
		else			lp.phi = lp.phi > 0. ? HALFPI : - HALFPI;
	else
		lp.phi = asin(lp.phi);
	return (lp);
}
FREEUP {  if (P) free(P); }
ENTRY(pj_mbtfps) {
	if (!P)
		return((PJ *)malloc(sizeof(PJ)));
	P->inv = s_inverse; P->fwd = s_forward; P->pfree = freeup;
	return P;
}
