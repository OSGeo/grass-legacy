static char *SCCSID = "@(#)stere.c	AMG v.1.1";
/* Stereographic projection and
** Universal Polar Stereographic projection */
# define	PROJ_MOD
# include	"projects.h"
struct _PROJ { PHEAD;
	double KS0, PHITS, PHI0;
	double E, SINX1, COSX1, AKM1;
	int MODE, SOUTH;
};
# define EPS10	1.e-10
# define TOL	1.e-8
# define NITER	8
# define CONV	1.e-10
# define S_POLE	0
# define N_POLE	1
# define OBLIQ	2
# define EQUIT	3
# define ks0	proj->KS0
# define phits	proj->PHITS
# define sinX1	proj->SINX1
# define cosX1	proj->COSX1
# define sinph0 proj->SINX1
# define cosph0 proj->COSX1
# define akm1	proj->AKM1
# define mode	proj->MODE
	static double
ssfn_(eccent, phit, sinphi) double eccent, phit, sinphi; {
	sinphi *= eccent;
	return (tan (.5 * (HALFPI + phit)) *
	   pow((1. - sinphi) / (1. + sinphi), .5 * eccent));
}
FORWARD(e_forward); /* ellipsoid */
	double coslam, sinlam, sinX, cosX, X, A, sinphi;

	coslam = cos(lam);
	sinlam = sin(lam);
	sinphi = sin(phi);
	if (mode == OBLIQ || mode == EQUIT) {
		sinX = sin(X = 2. * atan(ssfn_(e, phi, sinphi)) - HALFPI);
		cosX = cos(X);
	}
	switch (mode) {
	case OBLIQ:
		A = akm1 / (cosX1 * (1. + sinX1 * sinX +
		   cosX1 * cosX * coslam));
		y = A * (cosX1 * sinX - sinX1 * cosX * coslam);
		goto xmul;
	case EQUIT:
		A = 2. * akm1 / (1. + cosX * coslam);
		y = A * sinX;
xmul:
		x = A * cosX;
		break;
	case S_POLE:
		phi = -phi;
		coslam = - coslam;
		sinphi = -sinphi;
	case N_POLE:
		x = akm1 * tsfn_(e, phi, sinphi);
		y = - x * coslam;
		break;
	}
	x = x * sinlam;
	return (&xy);
}
INVERSE(e_inverse); /* ellipsoid */
	double cosphi, sinphi, tp, phi_l, rho, halfe, halfpi;
	int i;

	rho = hypot(x, y);
	switch (mode) {
	case OBLIQ:
	case EQUIT:
		cosphi = cos( tp = 2. * atan2(rho * cosX1 , akm1) );
		sinphi = sin(tp);
		phi_l = asin(cosphi * sinX1 + (y * sinphi * cosX1 / rho));
		tp = tan(.5 * (HALFPI + phi_l));
		x *= sinphi;
		y = rho * cosX1 * cosphi - y * sinX1* sinphi;
		halfpi = HALFPI;
		halfe = .5 * e;
		break;
	case S_POLE:
		phi = -phi;
		goto n_con;
	case N_POLE:
		y = -y;
n_con:
		phi_l = HALFPI - 2. * atan(tp = - rho / akm1);
		halfpi = -HALFPI;
		halfe = -.5 * e;
		break;
	}
	for (i = NITER; i--; phi_l = phi) {
		sinphi = e * sin(phi_l);
		phi = 2. * atan(tp * pow((1.+sinphi)/(1.-sinphi),
		   halfe)) - halfpi;
		if (fabs(phi_l - phi) < CONV) {
			if (mode == S_POLE)
				phi = -phi;
			lam = atan2(x, y);
			return (&lp);
		}
	}
	ERROR;
}
FORWARD(s_forward); /* spheroid */
	double  sinphi, cosphi, coslam, sinlam;

	sinphi = sin(phi);
	cosphi = cos(phi);
	coslam = cos(lam);
	sinlam = sin(lam);
	switch (mode) {
	case EQUIT:
		y = 1. + cosphi * coslam;
		goto oblcon;
	case OBLIQ:
		y = 1. + sinph0 * sinphi + cosph0 * cosphi * coslam;
oblcon:
		if (y <= EPS10) ERROR;
		x = (y = akm1 / y) * cosphi * sinlam;
		y *= (mode == EQUIT) ? sinphi :
		   cosph0 * sinphi - sinph0 * cosphi * coslam;
		break;
	case N_POLE:
		coslam = - coslam;
		phi = - phi;
	case S_POLE:
		if (fabs(phi - HALFPI) < TOL) ERROR;
		x = sinlam * ( y = akm1 * tan(FORTPI + .5 * phi) );
		y *= coslam;
		break;
	}
	return (&xy);
}
INVERSE(s_inverse); /* spheroid */
	double  coslam, c, cosphi, rh, sinc, cosc;

	sinc = sin(c = 2. * atan((rh = hypot(x, y)) / akm1));
	cosc = cos(c);
	lam = 0.;
	switch (mode) {
	case EQUIT:
		if (fabs(rh) <= EPS10)
			phi = 0.;
		else
			phi = asin(y * sinc / rh);
		if (cosc != 0. || x != 0.)
			lam = atan2(x * sinc, cosc * rh);
		break;
	case OBLIQ:
		if (fabs(rh) <= EPS10)
			phi = phi0;
		else
			phi = asin(cosc * sinph0 + y * sinc * cosph0 / rh);
		if ((c = cosc - sinph0 * sin(phi)) != 0. || x != 0.)
			lam = atan2(x * sinc * cosph0, c * rh);
		break;
	case N_POLE:
		y = -y;
	case S_POLE:
		if (fabs(rh) <= EPS10)
			phi = phi0;
		else
			phi = asin(mode == S_POLE ? - cosc : cosc);
		lam = atan2(x, y);
		break;
	}
	return (&lp);
}
	static void
setup(proj) struct _PROJ *proj; { /* general initialization */
	double t;

	if (fabs((t = fabs(phi0)) - HALFPI) < EPS10)
		mode = phi0 < 0. ? S_POLE : N_POLE;
	else
		mode = t > EPS10 ? OBLIQ : EQUIT;
	phits = fabs(phits);
	if (es) {
		double X;

		proj->forward = e_forward;
		proj->inverse = e_inverse;
		e = sqrt(es);
		switch (mode) {
		case N_POLE:
		case S_POLE:
			if (fabs(phits - HALFPI) < EPS10)
				akm1 = 2. * ks0 /
				   sqrt(pow(1+e,1+e)*pow(1-e,1-e));
			else {
				akm1 = cos(phits) /
				   tsfn_(e, phits, t = sin(phits));
				t *= e;
				akm1 /= sqrt(1. - t * t);
			}
			break;
		case EQUIT:
			akm1 = 2. * ks0;
			break;
		case OBLIQ:
			t = sin(phi0);
			X = 2. * atan(ssfn_(e, phi0, t)) - HALFPI;
			t *= e;
			akm1 = 2. * ks0 * cos(phi0) / sqrt(1. - t * t);
			sinX1 = sin(X);
			cosX1 = cos(X);
			break;
		}
	} else {
		proj->forward = s_forward;
		proj->inverse = s_inverse;
		switch (mode) {
		case OBLIQ:
			sinph0 = sin(phi0);
			cosph0 = cos(phi0);
		case EQUIT:
			akm1 = 2. * ks0;
			break;
		case S_POLE:
		case N_POLE:
			akm1 = fabs(phits - HALFPI) >= EPS10 ?
			   cos(phits) / tan(FORTPI - .5 * phits) :
			   2. * ks0 ;
			break;
		}
	}
	return;
}
ENTRY(stere) {
	if (! proj) return (sizeof(struct _PROJ));
	if (comset(I_ALL, proj, param)) ERROR;
	phi0 = *(*param)("rlat_0", "");
	ks0 = *(*param)("dk", "1.0");
	phits = *(*param)("rlat_ts", "90.");
	setup(proj);
	return(1);
}
ENTRY(ups) {
	if (! proj) return (sizeof(struct _PROJ));
	phi0 = *(int *)(*param)("bsouth") ? -HALFPI : HALFPI;
	/* International Ellipsoid */
	a = 6378388.000; es = 0.0067226700;
	ks0 = .994;
	x0 = 2000000.;
	y0 = 2000000.;
	phits = HALFPI;
	lam0 = 0.;
	setup(proj);
	return(1);
}
