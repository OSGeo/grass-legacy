/* dervative of (*P->fwd) projection */
#ifndef lint
static char RCSID[] = "@(#)$Id: pj_deriv.c,v 4.3 1992/07/14 01:28:57 gie Exp $";
#endif
#define __PJ_LIB
#include "projects.h"
static DERIVS der;

DERIVS *
#ifdef __STDC__
pj_deriv(LP lp, double h, PJ *P)
#else
pj_deriv(lp, h, P)
    LP lp;
    double h;
    PJ *P;
#endif
{
	XY t;

	lp.lam += h;
	lp.phi += h;
	if (fabs(lp.phi) > HALFPI) return (DERIVS *)0;
	h += h;
	t = (*P->fwd)(lp, P);
	if (t.x == HUGE_VAL) return (DERIVS *)0;
	der.p_x_l = t.x; der.p_y_p = t.y; der.p_x_p = -t.x; der.p_y_l = -t.y;
	lp.phi -= h;
	if (fabs(lp.phi) > HALFPI) return (DERIVS *)0;
	t = (*P->fwd)(lp, P);
	if (t.x == HUGE_VAL) return (DERIVS *)0;
	der.p_x_l += t.x; der.p_y_p -= t.y; der.p_x_p += t.x; der.p_y_l -= t.y;
	lp.lam -= h;
	t = (*P->fwd)(lp, P);
	if (t.x == HUGE_VAL) return (DERIVS *)0;
	der.p_x_l -= t.x; der.p_y_p -= t.y; der.p_x_p += t.x; der.p_y_l += t.y;
	lp.phi += h;
	t = (*P->fwd)(lp, P);
	if (t.x == HUGE_VAL) return (DERIVS *)0;
	der.p_x_l -= t.x; der.p_y_p += t.y; der.p_x_p -= t.x; der.p_y_l += t.y;
	der.p_x_l /= (h += h);
	der.p_y_p /= h;
	der.p_x_p /= h;
	der.p_y_l /= h;
	return &der;
}
