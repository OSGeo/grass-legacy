#ifndef lint
static char *SCCSID = "@(#)XYdense.c	USGS v.3.1";
#endif
/*  Denoyer Semi-Elliptical */
# include	"projects.h"
# define C0	0.95
# define C1	-.08333333333333333333
# define C3	.00166666666666666666
# define D1	0.9
# define D5	0.03
#ifdef FOR_CODE
FORWARD(s_forward); /* spheroid */
	y = phi;
	x = lam;
	lam = fabs(lam);
	x *= cos((C0 + lam * (C1 + lam * lam * C3)) *
		(phi * (D1 + D5 * phi * phi * phi * phi)));
	return (xy);
}
#else
NULL_FORWARD(s_forward);
#endif
#ifdef INV_CODE
NULL_INVERSE(s_inverse);
#else
NULL_INVERSE(s_inverse);
#endif
ENTRY(dense) { if (inverse) RETURN(s_inverse); else RETURN(s_forward); }
