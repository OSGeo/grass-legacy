#ifndef lint
static char *SCCSID = "@(#)UVeqc.c	USGS v.3.1";
#endif
/* Equidistant Cylindrical projection (Plate Caree) */
# include	"projects.h"
	static double
rc;
#ifdef FOR_CODE
FORWARD(s_forward); /* spheroid */
	x = rc * lam;
	y = phi;
	return (xy);
}
#else
NULL_FORWARD(s_forward);
#endif
#ifdef INV_CODE
INVERSE(s_inverse); /* spheroid */
	phi = y;
	lam = x / rc;
	return (lp);
}
#else
NULL_INVERSE(s_inverse);
#endif
ENTRY(eqc) {
	if ((rc = cos(*(*param)("rlat_ts", ""))) <= 0.) {
		emess(-1,"lat_ts >= 90d");
		E_ERROR;
	}
	if (inverse) RETURN(s_inverse); else RETURN(s_forward);
}
