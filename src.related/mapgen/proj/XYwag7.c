#ifndef lint
static char *SCCSID = "@(#)XYwag7.c	USGS v.3.1";
#endif
/*  Wagner VII Projection */
# include	"projects.h"
# define THIRD	0.3333333333333333333
#ifdef FOR_CODE
FORWARD(s_forward); /* spheroid */
	double s, c0, c1;

	s = 0.90631 * sin(phi);
	c0 = sqrt(1. - s * s);
	c1 = sqrt(2./(1. + c0 * cos(lam *= THIRD)));
	x = 2.66723 * c0 * c1 * sin(lam);
	y = 1.24104 * s * c1;
	return (xy);
}
#else
NULL_FORWARD(s_forward);
#endif
NULL_INVERSE(s_inverse);
ENTRY(wag7) { if (inverse) RETURN(s_inverse); else RETURN(s_forward); }
