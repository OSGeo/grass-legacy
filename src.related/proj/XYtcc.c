#ifndef lint
static char *SCCSID = "@(#)XYtcc.c	USGS v.3.1";
#endif
/* Transverse Central Cylindrical projection */
# include	"projects.h"
	static double
ap;
# define EPS10 1.e-10
#ifdef FOR_CODE
FORWARD(s_forward); /* spheroid */
	double b, bt;

	b = cos(phi) * sin(lam);
	if ((bt = 1. - b * b) < EPS10) F_ERROR;
	x = b / sqrt(bt);
	y = atan2(tan(phi) , cos(lam));
	return (xy);
}
#else
NULL_FORWARD(s_forward);
#endif
NULL_INVERSE(s_inverse);
ENTRY(tcc) { if (inverse) RETURN(s_inverse); else RETURN(s_forward); }
