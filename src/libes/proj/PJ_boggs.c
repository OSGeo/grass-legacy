/*  Boggs Eumorphic */
#ifndef lint
static char RCSID[] = "@(#)$Id: PJ_boggs.c,v 4.2 1992/07/14 01:27:10 gie Exp $";
#endif
#define __PJ_LIB
# include	"projects.h"
# define NITER	20
# define EPS	1e-7
# define ONETOL 1.000001
# define FXC	2.00276
# define FXC2	1.11072
# define FYC	0.49931
# define FYC2	1.41421356237309504880
FORWARD(s_forward) { XY xy;  /* spheroid */
	double theta, th1, c;
	int i;

	theta = lp.phi;
	if (fabs(fabs(lp.phi) - HALFPI) < EPS)
		xy.x = 0.;
	else {
		c = sin(theta) * PI;
		for (i = NITER; i; --i) {
			theta -= th1 = (theta + sin(theta) - c) /
				(1. + cos(theta));
			if (fabs(th1) < EPS) break;
		}
		theta *= 0.5;
		xy.x = FXC * lp.lam / (1. / cos(lp.phi) + FXC2 / cos(theta));
	}
	xy.y = FYC * (lp.phi + FYC2 * sin(theta));
	return (xy);
}
FREEUP {  if (P) free(P); }
ENTRY(pj_boggs) {
	if (!P)
		return((PJ *)malloc(sizeof(PJ)));
	P->inv = 0;
	P->fwd = s_forward;
	P->pfree = freeup;
	return P;
}
