#ifndef lint
static char *SCCSID = "@(#)XYbacon.c	USGS v.3.3";
#endif
/*  Bacon Globular, Apian Globular I and Ortelius Oval */
# include	"projects.h"
# define HLFPI2	2.46740110027233965467
# define EPS	1e-10
	static int
bacn, ortl;
#ifdef FOR_CODE
FORWARD(s_forward); /* spheroid */
	double ax, f;

	y = bacn ? HALFPI * sin(phi) : phi;
	if ((ax = fabs(lam)) >= EPS) {
		if (ortl && ax >= HALFPI)
			x = sqrt(HLFPI2 - phi * phi + EPS) + ax - HALFPI;
		else {
			f = 0.5 * (HLFPI2 / ax + ax);
			x = ax - f + sqrt(f * f - y * y);
		}
		if (lam < 0.) x = - x;
	} else
		x = 0.;
	return (xy);
}
#else
NULL_FORWARD(s_forward);
#endif
NULL_INVERSE(s_inverse);
ENTRY(bacon) {
	bacn = 1;
	ortl = 0;
	if (inverse) RETURN(s_inverse); else RETURN(s_forward);
}
ENTRY(apian) {
	bacn = ortl = 0;
	if (inverse) RETURN(s_inverse); else RETURN(s_forward);
}
ENTRY(ortel) {
	bacn = 0;
	ortl = 1;
	if (inverse) RETURN(s_inverse); else RETURN(s_forward);
}
