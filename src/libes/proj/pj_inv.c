
/* general inverse projection */
#ifndef lint
static char RCSID[] = "@(#)$Id: pj_inv.c,v 4.4 1992/07/14 01:29:00 gie Exp $";
#endif
#define __PJ_LIB
#include "projects.h"
# define EPS 1.0e-12


LP /* inverse projection entry */
#ifdef __STDC__
pj_inv(XY xy, PJ *P)
#else
pj_inv(xy, P)
    XY xy;
    PJ *P;
#endif
{
	LP lp;

	if (!P->inv) { /* check if inverse given */
		lp.lam = lp.phi = HUGE_VAL;
		return lp;
	}
	/* can't do preliminary checking as with forward */
	xy.x = (xy.x - P->x0) * P->ra; /* descale and de-offset */
	xy.y = (xy.y - P->y0) * P->ra;
	lp = (*P->inv)(xy, P); /* inverse project */
	if (lp.lam != HUGE_VAL) {
		if (pj_sfactors)
			pj_factors(lp, P);
		lp.lam += P->lam0; /* reduce from del lp.lam */
		if (!P->over)
			lp.lam = adjlon(lp.lam); /* adjust longitude to CM */
		if (P->geoc && fabs(fabs(lp.phi)-HALFPI) > EPS)
			lp.phi = atan(P->one_es * tan(lp.phi));
	}
	return lp;
}
