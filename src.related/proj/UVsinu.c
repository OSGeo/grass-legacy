#ifndef lint
static char *SCCSID = "@(#)UVsinu.c	USGS v.3.1";
#endif
/* Sinusoidal Projection */
# include	"projects.h"
# define EPS10	1e-10
#ifdef FOR_CODE
FORWARD(e_forward); /* ellipsoid */
	double s, c;

	y = mlfn_(phi, s = sin(phi), c = cos(phi));
	x = lam * c / sqrt(1. - es * s * s);
	return (xy);
}
FORWARD(s_forward); /* sphere */
	x = lam * cos(y = phi);
	return (xy);
}
#else
NULL_FORWARD(s_forward);
NULL_FORWARD(e_forward);
#endif
#ifdef INV_CODE
INVERSE(e_inverse); /* ellipsoid */
	double s;

	if ((s = fabs(phi = inv_mlfn_(y))) < HALFPI) {
		s = sin(phi);
		lam = x * sqrt(1. - es * s * s) / cos(phi);
	} else if ((s - EPS10) < HALFPI)
		lam = 0.;
	else I_ERROR;
	return (lp);
}
INVERSE(s_inverse); /* sphere */
	double s;

	if ((s = fabs(phi = y)) < HALFPI)
		lam = x / cos(phi);
	else if ((s - EPS10) < HALFPI)
		lam = 0.;
	else I_ERROR;
	return (lp);
}
#else
NULL_INVERSE(s_inverse);
NULL_INVERSE(e_inverse);
#endif
ENTRY(sinu) {
	if (es) {
		enfn_();
		if (inverse) RETURN(e_inverse); else RETURN(e_forward);
	} else	if (inverse) RETURN(s_inverse); else RETURN(s_forward);
}
