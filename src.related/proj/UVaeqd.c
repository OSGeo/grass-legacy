#ifndef lint
static char *SCCSID = "@(#)UVaeqd.c	USGS v.3.1";
#endif
/* Azimuthal Equidistant projection */
# include	"projects.h"
# define EPS10 1.e-10
	static double
sinph0, cosph0;
	static int
mode;
# define N_POLE	0
# define S_POLE 1
# define EQUIT	2
# define OBLIQ	3
#ifdef FOR_CODE
FORWARD(s_forward); /* spherical */
	double  coslam, cosphi, sinphi;

	sinphi = sin(phi);
	cosphi = cos(phi);
	coslam = cos(lam);
	switch (mode) {
	case EQUIT:
		y = cosphi * coslam;
		goto oblcon;
	case OBLIQ:
		y = sinph0 * sinphi + cosph0 * cosphi * coslam;
oblcon:
		if (fabs(fabs(y) - 1.) < EPS10) {
			if (y < 0.) F_ERROR;
			y = 1.;
		} else {
			y = acos(y);
			y /= sin(y);
		}
		x = y * cosphi * sin(lam);
		y *= (mode == EQUIT) ? sinphi :
		   cosph0 * sinphi - sinph0 * cosphi * coslam;
		break;
	case N_POLE:
		phi = -phi;
		coslam = -coslam;
	case S_POLE:
		if (fabs(phi - HALFPI) < EPS10) F_ERROR;
		x = (y = (HALFPI + phi)) * sin(lam);
		y *= coslam;
		break;
	}
	return (xy);
}
#else
NULL_FORWARD(s_forward);
#endif
#ifdef INV_CODE
INVERSE(s_inverse); /* spherical */
	double  coslam, cosphi, cosc, rh, sinc, c;

	if ((c = (rh = hypot(x, y))) > PI) I_ERROR;
	switch (mode) {
	case OBLIQ:
	case EQUIT:
		if (fabs(rh) <= EPS10)
			phi = phi0;
		else {
			sinc = sin(c);
			cosc = cos(c);
			if (mode == EQUIT) {
				phi = y * sinc / rh;
				if (fabs(phi) >= 1.)
					phi = phi > 0. ? HALFPI : - HALFPI;
				else
					phi = asin(phi);
				x *= sinc;
				y = cosc * rh;
			} else {
				phi = cosc * sinph0 + y * sinc * cosph0 / rh;
				if (fabs(phi) >= 1.)
					phi = phi > 0. ? HALFPI : - HALFPI;
				else
					phi = asin(phi);
				y = (cosc - sinph0 * sin(phi)) * rh;
				x *= sinc * cosph0;
			}
		}
		break;
	case N_POLE:
		y = -y;
		phi = HALFPI - c;
		break;
	case S_POLE:
		phi = c - HALFPI;
		break;
	}
	lam = (y == 0. && (mode == EQUIT || mode == OBLIQ)) ?
		0. : atan2(x, y);
	return (lp);
}
#else
NULL_INVERSE(s_inverse);
#endif
ENTRY(aeqd) {
	phi0 = *(*param)("rlat_0", "");
	if (fabs(fabs(phi0) - HALFPI) < EPS10)
		mode = phi0 < 0. ? S_POLE : N_POLE;
	else if (fabs(phi0) < EPS10)
		mode = EQUIT;
	else {
		mode = OBLIQ;
		sinph0 = sin(phi0);
		cosph0 = cos(phi0);
	}
	if (inverse) RETURN(s_inverse); else RETURN(s_forward);
}
