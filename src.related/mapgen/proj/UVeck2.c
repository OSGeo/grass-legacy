#ifndef lint
static char *SCCSID = "@(#)UVeck2.c	USGS v.3.1";
#endif
/*  Eckert II Projection */
# include	"projects.h"
# define FXC	0.46065886596178063902
# define FYC	1.44720250911653531871
# define C13	0.33333333333333333333
# define ONEEPS	1.0000001
#ifdef FOR_CODE
FORWARD(s_forward); /* spheroid */
	x = FXC * lam * (y = sqrt(4. - 3. * sin(fabs(phi))));
	y = FYC * (2. - y);
	if ( phi < 0.) y = -y;
	return (xy);
}
#else
NULL_FORWARD(s_forward);
#endif
#ifdef INV_CODE
INVERSE(s_inverse); /* spheroid */
	lam = x / (FXC * ( phi = 2. - fabs(y) / FYC) );
	phi = (4. - phi * phi) * C13;
	if (fabs(phi) >= 1.) {
		if (fabs(phi) > ONEEPS)	I_ERROR
		else
			phi = phi < 0. ? -HALFPI : HALFPI;
	} else
		phi = asin(phi);
	if (y < 0)
		phi = -phi;
	return (lp);
}
#else
NULL_INVERSE(s_inverse);
#endif
ENTRY(eck2) { if (inverse) RETURN(s_inverse); else RETURN(s_forward); }
