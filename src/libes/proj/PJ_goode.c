/*  Goode Homolosine Projection */
#ifndef lint
static char RCSID[] = "@(#)$Id: PJ_goode.c,v 4.2 1992/07/14 01:27:28 gie Exp $";
#endif
#define __PROJ_PARMS \
	struct PJconsts	*sinu; \
	struct PJconsts	*moll;
#define __PJ_LIB
#include	"projects.h"
	extern PJ
*pj_sinu PROTO((PJ *)), *pj_moll PROTO((PJ *));
#define Y_COR		0.05280
#define PHI_LIM	.71093078197902358062
FORWARD(s_forward) { XY xy;  /* spheroid */
	if (fabs(lp.phi) <= PHI_LIM)
		xy = P->sinu->fwd(lp, P->sinu);
	else {
		xy = P->moll->fwd(lp, P->sinu);
		xy.y -= lp.phi >= 0.0 ? Y_COR : -Y_COR;
	}
	return (xy);
}
FREEUP { 
	if (P) {
		if (P->sinu)
			(*(P->sinu->pfree))(P->sinu);
		if (P->moll)
			(*(P->moll->pfree))(P->moll);
		free(P);
	}
}
ENTRY(pj_goode) {
	if (!P)
		return((PJ *)malloc(sizeof(PJ)));
	if (P->es) {
		emess(-1,"only spherical form allowed");
		E_ERROR;
	}
	if (!(P->sinu = pj_sinu(0)) || !(P->moll = pj_moll(0)))
		E_ERROR;
	if (!(P->sinu = pj_sinu(P->sinu)) || !(P->moll = pj_moll(P->moll)))
		E_ERROR;
	P->inv = 0; P->fwd = s_forward; P->pfree = freeup;
	return P;
}
