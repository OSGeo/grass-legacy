#ifndef lint
static char *SCCSID = "@(#)UVeck1.c	USGS v.3.1";
#endif
/*  Eckert I Projection */
# include	"projects.h"
# define FC	.92131773192356127802
# define RP	.31830988618379067154
#ifdef FOR_CODE
FORWARD(s_forward); /* spheroid */
	x = FC * lam * (1. - RP * fabs(phi));
	y = FC * phi;
	return (xy);
}
#else
NULL_FORWARD(s_forward);
#endif
#ifdef INV_CODE
INVERSE(s_inverse); /* spheroid */
	phi = y / FC;
	lam = x / (FC * (1. - RP * fabs(phi)));
	return (lp);
}
#else
NULL_INVERSE(s_inverse);
#endif
ENTRY(eck1) { if (inverse) RETURN(s_inverse); else RETURN(s_forward); }
