#ifndef lint
static char *SCCSID = "@(#)XYaugust.c	USGS v.3.2";
#endif
/*  August Epicycloidal */
# include	"projects.h"
# undef INV_CODE
# define M 1.333333333333333
#ifdef FOR_CODE
FORWARD(s_forward); /* spheroid */
	double t, c1, c, x1, x12, y1, y12;

	t = tan(.5 * phi);
	c1 = sqrt(1. - t * t);
	c = 1. + c1 * cos(lam *= .5);
	x1 = sin(lam) *  c1 / c;
	y1 =  t / c;
	x = M * x1 * (3. + (x12 = x1 * x1) - 3. * (y12 = y1 *  y1));
	y = M * y1 * (3. + 3. * x12 - y12);
	return (xy);
}
#else
NULL_FORWARD(s_forward);
#endif
#ifdef INV_CODE
INVERSE(s_inverse); /* spheroid */
	return (lp);
}
#else
NULL_INVERSE(s_inverse);
#endif
ENTRY(august) {
	if (inverse) RETURN(s_inverse); else RETURN(s_forward);
}
