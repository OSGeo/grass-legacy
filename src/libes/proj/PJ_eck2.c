/*  Eckert II Projection */
#ifndef lint
static char RCSID[] = "@(#)$Id: PJ_eck2.c,v 4.2 1992/07/14 01:27:18 gie Exp $";
#endif
#define __PJ_LIB
# include	"projects.h"
#define FXC	0.46065886596178063902
#define FYC	1.44720250911653531871
#define C13	0.33333333333333333333
#define ONEEPS	1.0000001
FORWARD(s_forward) { XY xy;  /* spheroid */
	xy.x = FXC * lp.lam * (xy.y = sqrt(4. - 3. * sin(fabs(lp.phi))));
	xy.y = FYC * (2. - xy.y);
	if ( lp.phi < 0.) xy.y = -xy.y;
	return (xy);
}
INVERSE(s_inverse) { LP lp;  /* spheroid */
	lp.lam = xy.x / (FXC * ( lp.phi = 2. - fabs(xy.y) / FYC) );
	lp.phi = (4. - lp.phi * lp.phi) * C13;
	if (fabs(lp.phi) >= 1.) {
		if (fabs(lp.phi) > ONEEPS)	I_ERROR
		else
			lp.phi = lp.phi < 0. ? -HALFPI : HALFPI;
	} else
		lp.phi = asin(lp.phi);
	if (xy.y < 0)
		lp.phi = -lp.phi;
	return (lp);
}
FREEUP {  if (P) free(P); }
ENTRY(pj_eck2) {
	if (!P)
		return((PJ *)malloc(sizeof(PJ)));
	P->inv = s_inverse; P->fwd = s_forward; P->pfree = freeup;
	return P;
}
