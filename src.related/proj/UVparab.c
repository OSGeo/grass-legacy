#ifndef lint
static char *SCCSID = "@(#)UVparab.c	USGS v.3.2";
#endif
/*  Craster Parabolic Projection */
# include	"projects.h"
#define XM	0.97720502380583984317
#define RXM	1.02332670794648848847
#define YM	3.06998012383946546542
#define RYM	0.32573500793527994772
#define THIRD	0.333333333333333333
#ifdef FOR_CODE
FORWARD(s_forward); /* spheroid */
	phi *= THIRD;
	x = XM * lam * (2. * cos(phi + phi) - 1.);
	y = YM * sin(phi);
	return (xy);
}
#else
NULL_FORWARD(s_forward);
#endif
#ifdef INV_CODE
INVERSE(s_inverse); /* spheroid */
	phi = 3. * asin(y * RYM);
	lam = x * RXM / (2. * cos((phi + phi) * THIRD) - 1);
	return (lp);
}
#else
NULL_INVERSE(s_inverse);
#endif
ENTRY(parab) { if (inverse) RETURN(s_inverse); else RETURN(s_forward); }
