/*  Bacon Globular, Apian Globular I and Ortelius Oval */
#ifndef lint
static char RCSID[] = "@(#)$Id: PJ_bacon.c,v 4.2 1992/07/14 01:27:08 gie Exp $";
#endif
# define HLFPI2	2.46740110027233965467
# define EPS	1e-10
#define __PROJ_PARMS \
	int bacn; \
	int ortl;
#define __PJ_LIB
#include	"projects.h"
FORWARD(s_forward) { XY xy;  /* spheroid */
	double ax, f;

	xy.y = P->bacn ? HALFPI * sin(lp.phi) : lp.phi;
	if ((ax = fabs(lp.lam)) >= EPS) {
		if (P->ortl && ax >= HALFPI)
			xy.x = sqrt(HLFPI2 - lp.phi * lp.phi + EPS) + ax - HALFPI;
		else {
			f = 0.5 * (HLFPI2 / ax + ax);
			xy.x = ax - f + sqrt(f * f - xy.y * xy.y);
		}
		if (lp.lam < 0.) xy.x = - xy.x;
	} else
		xy.x = 0.;
	return (xy);
}
FREEUP {  if (P) free(P); }
ENTRY(pj_bacon) {
	if (!P)
		return((PJ *)malloc(sizeof(PJ)));
	P->bacn = 1;
	P->ortl = 0;
	P->inv = 0; P->fwd = s_forward; P->pfree = freeup;
	return P;
}
ENTRY(pj_apian) {
	if (!P)
		return((PJ *)malloc(sizeof(PJ)));
	P->bacn = P->ortl = 0;
	P->inv = 0; P->fwd = s_forward; P->pfree = freeup;
	return P;
}
ENTRY(pj_ortel) {
	if (!P)
		return((PJ *)malloc(sizeof(PJ)));
	P->bacn = 0;
	P->ortl = 1;
	P->inv = 0; P->fwd = s_forward; P->pfree = freeup;
	return P;
}
