#ifndef lint
static char *SCCSID = "@(#)XYairy.c	USGS v.3.4";
#endif
/* Airy projection */
# include	"projects.h"
	static double
p_halfpi, sinph0, cosph0, Cb;
	static int
no_cut = 0, /* do not cut at hemisphere limit */
mode;
# define EPS 1.e-10
# define N_POLE	0
# define S_POLE 1
# define EQUIT	2
# define OBLIQ	3
#ifdef FOR_CODE
FORWARD(s_forward); /* spheroid */
	double  sinlam, coslam, cosphi, sinphi, t, s, Krho, cosz;

	sinlam = sin(lam);
	coslam = cos(lam);
	switch (mode) {
	case EQUIT:
	case OBLIQ:
		sinphi = sin(phi);
		cosphi = cos(phi);
		cosz = cosphi * coslam;
		if (mode == OBLIQ)
			cosz = sinph0 * sinphi + cosph0 * cosz;
		if (!no_cut && cosz < -EPS)
			F_ERROR;
		if (fabs(s = 1. - cosz) > EPS) {
			t = 0.5 * (1. + cosz);
			Krho = -log(t)/s - Cb / t;
		} else
			Krho = 0.5 - Cb;
		x = Krho * cosphi * sinlam;
		if (mode == OBLIQ)
			y = Krho * (cosph0 * sinphi - sinph0 * cosphi * coslam);
		else
			y = Krho * sinphi;
		break;
	case S_POLE:
	case N_POLE:
		phi = fabs(p_halfpi - phi);
		if (!no_cut && (phi - EPS) > HALFPI)
			F_ERROR;
		if ((phi *= 0.5) > EPS) {
			t = tan(phi);
			Krho = -2.*(log(cos(phi)) / t + t * Cb);
			x = Krho * sinlam;
			y = Krho * coslam;
			if (mode == N_POLE)
				y = -y;
		} else
			x = y = 0.;
	}
	return (xy);
}
#else
NULL_FORWARD(s_forward);
#endif
NULL_INVERSE(s_inverse);
ENTRY(airy) {
	double beta;

	no_cut = *(int *)(*param)("bno_cut", "");
	beta = 0.5 * (HALFPI - *(*param)("rlat_b", "90.0"));
	if (fabs(beta) < EPS)
		Cb = -0.5;
	else {
		Cb = 1./tan(beta);
		Cb *= Cb * log(cos(beta));
	}
	if (fabs(fabs(phi0) - HALFPI) < EPS)
		if (phi0 < 0.) {
			p_halfpi = -HALFPI;
			mode = S_POLE;
		} else {
			p_halfpi =  HALFPI;
			mode = N_POLE;
		}
	else {
		if (fabs(phi0) < EPS)
			mode = EQUIT;
		else {
			mode = OBLIQ;
			sinph0 = sin(phi0);
			cosph0 = cos(phi0);
		}
	}
	if (inverse) RETURN(s_inverse); else RETURN(s_forward);
}
