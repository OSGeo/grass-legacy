static char *SCCSID = "@(#)ortho.c	AMG v.1.1";
/* Orthographic projection */
# define	PROJ_MOD
# include	"projects.h"
struct _PROJ { PHEAD;
	double PHI0, SINPH0, COSPH0;
	int	MODE;
};
# define EPS10 1.e-10
# define sinph0	proj->SINPH0
# define cosph0	proj->COSPH0
# define mode	proj->MODE
# define N_POLE	0
# define S_POLE 1
# define EQUIT	2
# define OBLIQ	3
FORWARD(s_forward); /* spheroid */
	double  coslam, cosphi, sinphi;

	cosphi = cos(phi);
	coslam = cos(lam);
	switch (mode) {
	case EQUIT:
		if (cosphi * coslam < - EPS10) ERROR;
		y = sin(phi);
		break;
	case OBLIQ:
		if (sinph0 * (sinphi = sin(phi)) +
		   cosph0 * cosphi * coslam < - EPS10) ERROR;
		y = cosph0 * sinphi - sinph0 * cosphi * coslam;
		break;
	case N_POLE:
		coslam = - coslam;
	case S_POLE:
		if (fabs(phi - phi0) - EPS10 > HALFPI) ERROR;
		y = cosphi * coslam;
		break;
	}
	x = cosphi * sin(lam);
	return (&xy);
}
INVERSE(s_inverse); /* spheroid */
	double  rh, cosphi, sinphi, cosc, sinc;

	if ((sinc = (rh = hypot(x, y))) > 1.) {
		if ((sinc - 1.) > EPS10) ERROR;
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
	return (&lp);
}
ENTRY(ortho) {
	if (! proj) return (sizeof(struct _PROJ));
	if (comset(I_LAM0 + I_XY, proj, param)) ERROR;
	phi0 = *(*param)("rlat_0", "");
	proj->forward = s_forward;
	proj->inverse = s_inverse;
	if (fabs(fabs(phi0) - HALFPI) <= EPS10)
		mode = phi0 < 0. ? S_POLE : N_POLE;
	else if (fabs(phi0) > EPS10) {
		mode = OBLIQ;
		sinph0 = sin(phi0);
		cosph0 = cos(phi0);
	} else
		mode = EQUIT;
	return (1);
}
