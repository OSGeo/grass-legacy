#ifndef lint
static char *SCCSID = "@(#)UVeck5.c	USGS v.3.1";
#endif
/*  Eckert V */
# include	"projects.h"
#define XF	0.44101277172455148219
#define RXF	2.26750802723822639137
#define YF	0.88202554344910296438
#define RYF	1.13375401361911319568
#ifdef FOR_CODE
FORWARD(s_forward); /* spheroid */
	x = XF * (1. + cos(phi)) * lam;
	y = YF * phi;
	return (xy);
}
#else
NULL_FORWARD(s_forward);
#endif
#ifdef INV_CODE
INVERSE(s_inverse); /* spheroid */
	lam = RXF * x / (1. + cos( phi = RYF * y));
	return (lp);
}
#else
NULL_INVERSE(s_inverse);
#endif
ENTRY(eck5) {
	if (inverse) RETURN(s_inverse); else RETURN(s_forward);
}
