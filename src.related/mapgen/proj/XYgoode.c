#ifndef lint
static char *SCCSID = "@(#)XYgoode.c	USGS v.3.1";
#endif
/*  Goode Homolosine */
# include	"projects.h"
#undef INV_CODE
	extern XY
(*sinu())(), (*moll())();
	static XY
(*SINU)(), (*MOLL)();
# define Y_COR		0.05280
# define PHI_LIM	.71093078197902358062
#ifdef FOR_CODE
FORWARD(s_forward); /* spheroid */
	if (fabs(phi) <= PHI_LIM)
		xy = (*SINU)(lp);
	else {
		xy = (*MOLL)(lp);
		y -= phi >= 0.0 ? Y_COR : -Y_COR;
	}
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
ENTRY(goode) {
	es = 0.; /* force to sphere */
	if (!(SINU = sinu(0)) || !(MOLL = moll(0))) E_ERROR;
	if (inverse) RETURN(s_inverse); else RETURN(s_forward);
}
