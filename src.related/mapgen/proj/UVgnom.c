#ifndef lint
static char *SCCSID = "@(#)UVgnom.c	USGS v.3.1";
#endif
/* Gnomonic projection */
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

	sinphi = sin(phi);
	cosphi = cos(phi);
	coslam = cos(lam);
	switch (mode) {
	case EQUIT:
		y = cosphi * coslam;
		break;
	case OBLIQ:
		y = sinph0 * sinphi + cosph0 * cosphi * coslam;
		break;
	case S_POLE:
		y = - sinphi;
		break;
	case N_POLE:
		y = sinphi;
		break;
	}
	if (y <= EPS10) F_ERROR;
	x = (y = 1. / y) * cosphi * sin(lam);
	switch (mode) {
	case EQUIT:
		y *= sinphi;
		break;
	case OBLIQ:
		y *= cosph0 * sinphi - sinph0 * cosphi * coslam;
		break;
	case N_POLE:
		coslam = - coslam;
	case S_POLE:
		y *= cosphi * coslam;
		break;
	}
	return (xy);
}
#else
NULL_FORWARD(s_forward);
#endif
#ifdef INV_CODE
INVERSE(s_inverse); /* spheroid */
	double  rh, cosz, sinz;

	rh = hypot(x, y);
	sinz = sin(phi = atan(rh));
	cosz = sqrt(1. - sinz * sinz);
	if (fabs(rh) <= EPS10) {
		phi = phi0;
		lam = 0.;
	} else {
		switch (mode) {
		case OBLIQ:
			phi = cosz * sinph0 + y * sinz * cosph0 / rh;
			if (fabs(phi) >= 1.)
				phi = phi > 0. ? HALFPI : - HALFPI;
			else
				phi = asin(phi);
			y = (cosz - sinph0 * sin(phi)) * rh;
			x *= sinz * cosph0;
			break;
		case EQUIT:
			phi = y * sinz / rh;
			if (fabs(phi) >= 1.)
				phi = phi > 0. ? HALFPI : - HALFPI;
			else
				phi = asin(phi);
			y = cosz * rh;
			x *= sinz;
			break;
		case S_POLE:
			phi -= HALFPI;
			break;
		case N_POLE:
			phi = HALFPI - phi;
			y = -y;
			break;
		}
		lam = atan2(x, y);
	}
	return (lp);
}
#else
NULL_INVERSE(s_inverse);
#endif
ENTRY(gnom) {
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
