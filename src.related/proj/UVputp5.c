#ifndef lint
static char *SCCSID = "@(#)UVputp5.c	USGS v.3.1";
#endif
/*  Putnins P5 Projection */
# include	"projects.h"
# define FXC	1.01346
# define FYC	1.01346
# define CS	1.21585420370805325734
#ifdef FOR_CODE
FORWARD(s_forward); /* spheroid */
	x = FXC * lam * (2. - sqrt(1. + CS * phi * phi));
	y = FYC * phi;
	return (xy);
}
#else
NULL_FORWARD(s_forward);
#endif
#ifdef INV_CODE
INVERSE(s_inverse); /* spheroid */
	phi = y / FYC;
	lam = x / (FXC * (2. - sqrt(1. + CS * phi * phi)));
	return (lp);
}
#else
NULL_INVERSE(s_inverse);
#endif
ENTRY(putp5) { if (inverse) RETURN(s_inverse); else RETURN(s_forward); }
