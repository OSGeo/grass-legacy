/* general forward projection */
#ifndef lint
static char RCSID[] = "@(#)$Id: pj_fwd.c,v 4.4 1992/07/14 01:28:58 gie Exp $";
#endif
#define __PJ_LIB
#include "projects.h"
# define EPS 1.0e-12

XY /* forward projection entry */
#ifdef __STDC__
pj_fwd(LP lp, PJ *P)
#else
pj_fwd(lp, P)
    LP lp;
    PJ *P;
#endif
{
	XY xy;
	double t;

	/* check for forward and latitude or longitude overange */
	if (!P->fwd || (t = fabs(lp.phi)-HALFPI) > EPS) {
		xy.x = xy.y = HUGE_VAL;
		return xy;
	}
	if (fabs(t) <= EPS)
		lp.phi = lp.phi < 0. ? -HALFPI : HALFPI;
	else if (P->geoc)
		lp.phi = atan(P->rone_es * tan(lp.phi));
	lp.lam -= P->lam0;	/* compute del lp.lam */
	if (!P->over)
		lp.lam = adjlon(lp.lam); /* adjust del longitude */
	xy = (*P->fwd)(lp, P); /* project */
	/* adjust for major axis and easting/northings */
	if (xy.x != HUGE_VAL) {
		if (pj_sfactors)
			pj_factors(lp, P);
		xy.x = P->a * xy.x + P->x0;
		xy.y = P->a * xy.y + P->y0;
	}
	return xy;
}
