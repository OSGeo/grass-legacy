#ifndef lint
static char *SCCSID = "@(#)UVortho.c	USGS v.3.1";
#endif
/* Orthographic projection */
# include	"projects.h"
	static double
sinph0, cosph0;
	static int
mode;
# define EPS10 1.e-10
# define N_POLE	0
# define S_POLE 1
# define EQUIT	2
# define OBLIQ	3
#ifdef FOR_CODE
FORWARD(s_forward); /* spheroid */
	double  coslam, cosphi, sinphi;

	cosphi = cos(phi);
	coslam = cos(lam);
	switch (mode) {
	case EQUIT:
		if (cosphi * coslam < - EPS10) F_ERROR;
		y = sin(phi);
		break;
	case OBLIQ:
		if (sinph0 * (sinphi = sin(phi)) +
		   cosph0 * cosphi * coslam < - EPS10) F_ERROR;
		y = cosph0 * sinphi - sinph0 * cosphi * coslam;
		break;
	case N_POLE:
		coslam = - coslam;
	case S_POLE:
		if (fabs(phi - phi0) - EPS10 > HALFPI) F_ERROR;
		y = cosphi * coslam;
		break;
	}
	x = cosphi * sin(lam);
	return (xy);
}
#else
NULL_FORWARD(s_forward);
#endif
#ifdef INV_CODE
INVERSE(s_inverse); /* spheroid */
	double  rh, cosphi, sinphi, cosc, sinc;

	if ((sinc = (rh = hypot(x, y))) > 1.) {
		if ((sinc - 1.) > EPS10) I_ERROR;
		sinc = 1.;
	}
	cosc = sqrt(1. - sinc * sinc); /* in this range OK */
	if (fabs(rh) <= EPS10)
		phi = phi0;
	else switch (mode) {
	case N_POLE:
		y = -y;
		phi = acos(sinc);
		break;
	case S_POLE:
		phi = - acos(sinc);
		break;
	case EQUIT:
		phi = y * sinc / rh;
		x *= sinc;
		y = cosc * rh;
		goto sinchk;
	case OBLIQ:
		phi = cosc * sinph0 + y * sinc * cosph0 / rh;
		y = (cosc - sinph0 * phi) * rh;
		x *= sinc * cosph0;
sinchk:
		if (fabs(phi) >= 1.)
			phi = phi < 0. ? -HALFPI : HALFPI;
		else
			phi = asin(phi);
		break;
	}
	lam = (y == 0. && (mode == OBLIQ || mode == EQUIT)) ?
		 (x == 0. ? 0. : x < 0. ? -HALFPI : HALFPI) : atan2(x, y);
	return (lp);
}
#else
NULL_INVERSE(s_inverse);
#endif
ENTRY(ortho) {
	if (fabs(fabs(phi0) - HALFPI) <= EPS10)
		mode = phi0 < 0. ? S_POLE : N_POLE;
	else if (fabs(phi0) > EPS10) {
		mode = OBLIQ;
		sinph0 = sin(phi0);
		cosph0 = cos(phi0);
	} else
		mode = EQUIT;
	if (inverse) RETURN(s_inverse); else RETURN(s_forward);
}
