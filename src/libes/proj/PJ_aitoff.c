/*  Aitoff and Winkel Tripel Projections */
#ifndef lint
static char RCSID[] = "@(#)$Id: PJ_aitoff.c,v 4.2 1992/07/14 01:27:06 gie Exp $";
#endif
#define __PROJ_PARMS \
	double	cosphi1; \
	int		mode;
#define EPS	1e-8
#define __PJ_LIB
#include	"projects.h"
FORWARD(s_forward) { XY xy;  /* spheroid */
	double c, d;

	if (d = acos(cos(lp.phi) * cos(0.5 * lp.lam))) { /* basic Aitoff */
		c = sin(lp.phi) / sin(d);
		if ((xy.x = 1. - c * c) < EPS)
			if (xy.x < -EPS) F_ERROR
			else xy.x = 0.;
		else
			xy.x = 2. * d * sqrt(xy.x);
		if (lp.lam < 0.) xy.x = - xy.x;
		xy.y = d * c;
	} else
		xy.x = xy.y = 0.;
	if (P->mode) { /* Winkel Tripel */
		xy.x = (xy.x + lp.lam * P->cosphi1) * 0.5;
		xy.y = (xy.y + lp.phi) * 0.5;
	}
	return (xy);
}
FREEUP {  if (P) free(P); }

static PJ *
#ifdef __STDC__
  setup(PJ *P)
#else
  setup(P)
      PJ *P;
#endif
{
	P->inv = 0;
	P->fwd = s_forward;
	P->pfree = freeup;
	return P;
}
ENTRY(pj_aitoff) {
	if (!P)
		return((PJ *)malloc(sizeof(PJ)));
	P->mode = 0;
	return setup(P);
}
ENTRY(pj_wintri) {
	if (!P)
		return((PJ *)malloc(sizeof(PJ)));
	P->mode = 1;
	if ((P->cosphi1 = cos(pj_param("rlat_1", "50.467")->f)) == 0.) {
		emess(1,"cos(lat_1) == 0.");
		E_ERROR;
	}
	return setup(P);
}
