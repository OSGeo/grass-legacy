/* Perspective Conic */
#ifndef lint
static char RCSID[] = "@(#)$Id: PJ_pconic.c,v 4.2 1992/07/14 01:27:48 gie Exp $";
#endif
#define __PROJ_PARMS \
	double	phi1; \
	double	phi2; \
	double	n; \
	double	rho; \
	double	rho0; \
	double	c2; \
	double	check; \
	int		negative;
#define __PJ_LIB
# include	"projects.h"
# define EPS10	1.e-10
FORWARD(s_forward) { XY xy;  /* sphere & ellipsoid */
	
	if ((P->negative && lp.phi >= P->check) ||
	    (!P->negative && lp.phi <= P->check)) F_ERROR;
	P->rho = P->rho0 - P->c2 * tan(lp.phi - P->phi0);
	xy.x = P->rho * sin( lp.lam *= P->n );
	xy.y = P->rho0 - P->rho * cos(lp.lam);
	return (xy);
}
FREEUP {  if (P) free(P); }
ENTRY(pj_pconic) {
	double cosphi, sinphi;

	if (!P)
		return((PJ *)malloc(sizeof(PJ)));
	P->phi1 = pj_param("rlat_1", "29.5")->f;
	P->phi2 = pj_param("rlat_2", "45.5")->f;
	if (fabs(P->phi0 = 0.5 * (P->phi1 + P->phi2)) < EPS10) {
		emess(-1,"lat_1 = - lat_2");
		E_ERROR;
	}
	P->check = P->phi0 + ((P->negative = P->phi0 < 0.) ? HALFPI : -HALFPI);
	P->n = sinphi = sin(P->phi0);
	cosphi = cos(P->phi0);
	P->c2 = cos(0.5 * (P->phi2 - P->phi1));
	P->rho0 = P->c2 * cosphi / sinphi;
	P->inv = 0; P->fwd = s_forward; P->pfree = freeup;
	return P;
}
