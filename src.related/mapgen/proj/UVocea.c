#ifndef lint
static char *SCCSID = "@(#)UVocea.c	USGS v.3.1";
#endif
/*  Oblique Cylindrical Equal Area */
# include	"projects.h"
	static double
rok, rtk, sinphi, cosphi, singam, cosgam;
#ifdef FOR_CODE
FORWARD(s_forward); /* spheroid */
	double t;

	y = sin(lam);
/*
	x = atan2((tan(phi) * cosphi + sinphi * y) , cos(lam));
*/
	t = cos(lam);
	x = atan((tan(phi) * cosphi + sinphi * y) / t);
	if (t < 0.)
		x += PI;
	x *= rtk;
	y = rok * (sinphi * sin(phi) - cosphi * cos(phi) * y);
	return (xy);
}
#else
NULL_FORWARD(s_forward);
#endif
#ifdef INV_CODE
INVERSE(s_inverse); /* spheroid */
	double t, s;

	y /= rok;
	x /= rtk;
	t = sqrt(1. - y * y);
	phi = asin(y * sinphi + t * cosphi * (s = sin(x)));
	lam = atan2(t * sinphi * s - y * cosphi,
		t * cos(x));
	return (lp);
}
#else
NULL_INVERSE(s_inverse);
#endif
ENTRY(ocea) {
	double k_0, phi_0, phi_1, phi_2, lam_1, lam_2, lonz, alpha;

	if ((k_0 = *(*param)("dk", "1.")) <= 0.) {
		emess(-1,"k <= 0");
		E_ERROR;
	}
	rok = a / k_0;
	rtk = a * k_0;
	phi_0 = *(*param)("rlat_0","");
	phi_1 = *(*param)("rlat_1","");
	phi_2 = *(*param)("rlat_2","");
	lam_1 = *(*param)("rlon_1","");
	lam_2 = *(*param)("rlon_2","");
	lonz = *(*param)("rlonc","");
	alpha	= *(*param)("ralpha",	"");
	if ( *(int *)(*param)("bazi","") ) {
		singam = atan(-cos(alpha)/(-sin(phi_0) * sin(alpha))) + lonz;
		sinphi = asin(cos(phi_0) * sin(alpha));
	} else {
		singam = atan2(cos(phi_1) * sin(phi_2) * cos(lam_1) -
			sin(phi_1) * cos(phi_2) * cos(lam_2),
			sin(phi_1) * cos(phi_2) * sin(lam_2) -
			cos(phi_1) * sin(phi_2) * sin(lam_1) );
		sinphi = atan(-cos(singam - lam_1) / tan(phi_1));
	}
	lam0 = singam + HALFPI;
	cosphi = cos(sinphi);
	sinphi = sin(sinphi);
	cosgam = cos(singam);
	singam = sin(singam);
	if (inverse) RETURN(s_inverse); else RETURN(s_forward);
}
