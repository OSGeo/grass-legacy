static char *SCCSID = "@(#)gnom.c	AMG v.1.1";
/* Gnomonic projection */
# define	PROJ_MOD
# include	"projects.h"
struct _PROJ { PHEAD;
	double	PHI0, SINPH0, COSPH0;
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
	if (y <= EPS10) ERROR;
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
	return (&xy);
}
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
	return (&lp);
}
ENTRY(gnom) {
	if (! proj) return (sizeof(struct _PROJ));
	if (comset(I_LAM0 + I_XY, proj, param)) ERROR;
	phi0 = *(*param)("rlat_0", "");
	proj->forward = s_forward;
	proj->inverse = s_inverse;
	if (fabs(fabs(phi0) - HALFPI) < EPS10)
		mode = phi0 < 0. ? S_POLE : N_POLE;
	else if (fabs(phi0) < EPS10)
		mode = EQUIT;
	else {
		mode = OBLIQ;
		sinph0 = sin(phi0);
		cosph0 = cos(phi0);
	}
	return (1);
}
