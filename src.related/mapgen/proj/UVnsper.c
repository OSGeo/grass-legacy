#ifndef lint
static char *SCCSID = "@(#)UVnsper.c	USGS v.3.1";
#endif
/* Near-sided perspective */
# include	"projects.h"
	static double
height, sinph0, cosph0, p, rp, pn1, pfact, h, cg, sg, sw, cw;
	static int
mode, tilt;
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
	case OBLIQ:
		y = sinph0 * sinphi + cosph0 * cosphi * coslam;
		break;
	case EQUIT:
		y = cosphi * coslam;
		break;
	case S_POLE:
		y = - sinphi;
		break;
	case N_POLE:
		y = sinphi;
		break;
	}
	if (y < rp) F_ERROR;
	y = pn1 / (p - y);
	x = y * cosphi * sin(lam);
	switch (mode) {
	case OBLIQ:
		y *= (cosph0 * sinphi -
		   sinph0 * cosphi * coslam);
		break;
	case EQUIT:
		y *= sinphi;
		break;
	case N_POLE:
		coslam = - coslam;
	case S_POLE:
		y *= cosphi * coslam;
		break;
	}
	if (tilt) {
		double yt, ba;

		yt = y * cg + x * sg;
		ba = 1. / (yt * sw * h + cw);
		x = (x * cg - y * sg) * cw * ba;
		y = yt * ba;
	}
	return (xy);
}
#else
NULL_FORWARD(s_forward);
#endif
#ifdef INV_CODE
INVERSE(s_inverse); /* spheroid */
	double  rh, cosz, sinz;

	if (tilt) {
		double bm, bq, yt;

		yt = 1./(pn1 - y * sw);
		bm = pn1 * x * yt;
		bq = pn1 * y * cw * yt;
		x = bm * cg + bq * sg;
		y = bq * cg - bm * sg;
	}
	rh = hypot(x, y);
	if ((sinz = 1. - rh * rh * pfact) < 0.) I_ERROR;
	sinz = (p - sqrt(sinz)) / (pn1 / rh + rh / pn1);
	cosz = sqrt(1. - sinz * sinz);
	if (fabs(rh) <= EPS10) {
		lam = 0.;
		phi = phi0;
	} else {
		switch (mode) {
		case OBLIQ:
			phi = asin(cosz * sinph0 + y * sinz * cosph0 / rh);
			y = (cosz - sinph0 * sin(phi)) * rh;
			x *= sinz * cosph0;
			break;
		case EQUIT:
			phi = asin(y * sinz / rh);
			y = cosz * rh;
			x *= sinz;
			break;
		case N_POLE:
			phi = asin(cosz);
			y = -y;
			break;
		case S_POLE:
			phi = - asin(cosz);
			break;
		}
		lam = atan2(x, y);
	}
	return (lp);
}
#else
NULL_INVERSE(s_inverse);
#endif
	static
ENTRY(setup) {
	if ((height = *(*param)("dh", "")) <= 0.) {
		emess(-1,"h <= 0.");
		E_ERROR;
	}
	if (fabs(fabs(phi0) - HALFPI) < EPS10)
		mode = phi0 < 0. ? S_POLE : N_POLE;
	else if (fabs(phi0) < EPS10)
		mode = EQUIT;
	else {
		mode = OBLIQ;
		sinph0 = sin(phi0);
		cosph0 = cos(phi0);
	}
	pn1 = height / a; /* normalize by radius */
	p = 1. + pn1;
	rp = 1. / p;
	h = 1. / pn1;
	pfact = (p + 1.) * h;
	if (inverse) RETURN(s_inverse); else RETURN(s_forward);
}
ENTRY(nsper) {
	tilt = 0;
	return (setup(inverse));
}
ENTRY(tpers) {
	double omega, gamma;

	omega = *(*param)("dtilt", "") * DEG_TO_RAD;
	gamma = *(*param)("dazi", "") * DEG_TO_RAD;
	tilt = 1;
	cg = cos(gamma); sg = sin(gamma);
	cw = cos(omega); sw = sin(omega);
	return(setup(inverse));
}
