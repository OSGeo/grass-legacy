#ifndef lint
static char *SCCSID = "@(#)XYpconic.c	USGS v.3.1";
#endif
/* Perspective Conic */
# include	"projects.h"
	static double
phi1, phi2, n, rho, rho0, c2, check;
	static int
negative;
# define EPS10	1.e-10
#ifdef FOR_CODE
FORWARD(s_forward); /* sphere & ellipsoid */
	
	if ((negative && phi >= check) ||
	    (!negative && phi <= check)) F_ERROR;
	rho = rho0 - c2 * tan(phi - phi0);
	x = rho * sin( lam *= n );
	y = rho0 - rho * cos(lam);
	return (xy);
}
#else
NULL_FORWARD(s_forward);
#endif
NULL_INVERSE(s_inverse);
ENTRY(pconic) {
	double cosphi, sinphi;

	phi1 = *(*param)("rlat_1", "29.5");
	phi2 = *(*param)("rlat_2", "45.5");
	if (fabs(phi0 = 0.5 * (phi1 + phi2)) < EPS10) {
		emess(-1,"lat_1 = - lat_2");
		E_ERROR;
	}
	check = phi0 + ((negative = phi0 < 0.) ? HALFPI : -HALFPI);
	n = sinphi = sin(phi0);
	cosphi = cos(phi0);
	c2 = cos(0.5 * (phi2 - phi1));
	rho0 = c2 * cosphi / sinphi;
	if (inverse) RETURN(s_inverse); else RETURN(s_forward);
}
