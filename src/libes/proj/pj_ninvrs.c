/* numeric inverse */
#ifndef lint
static char RCSID[] = "@(#)$Id: pj_ninvrs.c,v 4.2 1992/07/14 01:29:03 gie Exp $";
#endif
#define __PJ_LIB
#include "projects.h"
#define MAX_ITER 10
#define TOL 1e-8
LP
#ifdef __STDC__
pj_ninvrs(XY xy, LP lp, PJ *P)
#else
pj_ninvrs(xy, lp, P)
    XY xy;
    LP lp;
    PJ *P;
#endif
{
	XY	gxy;
	DERIVS *der;
	double den, dl, dp, fx, fy;
	int iter = MAX_ITER;

	do {
		if (!(der = pj_deriv(lp, 1e-7, P)))
			break;
		gxy = pj_fwd(lp, P);
		if (!(den = der->p_y_l * der->p_x_p - der->p_x_l * der->p_y_p))
			break;
		fx = xy.x - gxy.x;
		fy = xy.y - gxy.y;
		lp.phi -= dp = (der->p_x_l * fy - der->p_y_l * fx) / den;
		lp.lam -= dl = (der->p_y_p * fx - der->p_x_p * fy) / den;
		if (fabs(dp) < TOL || fabs(dl) < TOL)
			return lp;
	} while (iter--);
	lp.lam = lp.phi = HUGE_VAL;
	return lp;
}
