#ifndef lint
static char *SCCSID = "@(#)UVstere.c	USGS v.3.4";
#endif
/* Stereographic projection and
** Universal Polar Stereographic projection */
# include	"projects.h"
	static double
ks0, phits, sinX1, cosX1, akm1;
	static int
mode;
# define sinph0	sinX1
# define cosph0	cosX1
# define EPS10	1.e-10
# define TOL	1.e-8
# define NITER	8
# define CONV	1.e-10
# define S_POLE	0
# define N_POLE	1
# define OBLIQ	2
# define EQUIT	3
	static double
ssfn_(phit, sinphi) double phit, sinphi; {
	sinphi *= e;
	return (tan (.5 * (HALFPI + phit)) *
	   pow((1. - sinphi) / (1. + sinphi), .5 * e));
}
#ifdef FOR_CODE
FORWARD(e_forward); /* ellipsoid */
	double coslam, sinlam, sinX, cosX, X, A, sinphi;

	coslam = cos(lam);
	sinlam = sin(lam);
	sinphi = sin(phi);
	if (mode == OBLIQ || mode == EQUIT) {
		sinX = sin(X = 2. * atan(ssfn_(phi, sinphi)) - HALFPI);
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
		x = akm1 * tsfn_(phi, sinphi);
		y = - x * coslam;
		break;
	}
	x = x * sinlam;
	return (xy);
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
		if (y <= EPS10) F_ERROR;
		x = (y = akm1 / y) * cosphi * sinlam;
		y *= (mode == EQUIT) ? sinphi :
		   cosph0 * sinphi - sinph0 * cosphi * coslam;
		break;
	case N_POLE:
		coslam = - coslam;
		phi = - phi;
	case S_POLE:
		if (fabs(phi - HALFPI) < TOL) F_ERROR;
		x = sinlam * ( y = akm1 * tan(FORTPI + .5 * phi) );
		y *= coslam;
		break;
	}
	return (xy);
}
#else
NULL_FORWARD(s_forward);
NULL_FORWARD(e_forward);
#endif
#ifdef INV_CODE
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
			return (lp);
		}
	}
	I_ERROR;
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
	return (lp);
}
#else
NULL_INVERSE(s_inverse);
NULL_INVERSE(e_inverse);
#endif
	static
ENTRY(setup) { /* general initialization */
	double t;

	if (fabs((t = fabs(phi0)) - HALFPI) < EPS10)
		mode = phi0 < 0. ? S_POLE : N_POLE;
	else
		mode = t > EPS10 ? OBLIQ : EQUIT;
	phits = fabs(phits);
	if (es) {
		double X;

		switch (mode) {
		case N_POLE:
		case S_POLE:
			if (fabs(phits - HALFPI) < EPS10)
				akm1 = 2. * ks0 /
				   sqrt(pow(1+e,1+e)*pow(1-e,1-e));
			else {
				akm1 = cos(phits) /
				   tsfn_(phits, t = sin(phits));
				t *= e;
				akm1 /= sqrt(1. - t * t);
			}
			break;
		case EQUIT:
			akm1 = 2. * ks0;
			break;
		case OBLIQ:
			t = sin(phi0);
			X = 2. * atan(ssfn_(phi0, t)) - HALFPI;
			t *= e;
			akm1 = 2. * ks0 * cos(phi0) / sqrt(1. - t * t);
			sinX1 = sin(X);
			cosX1 = cos(X);
			break;
		}
		if (inverse) RETURN(e_inverse); else RETURN(e_forward);
	} else {
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
		if (inverse) RETURN(s_inverse); else RETURN(s_forward);
	}
}
ENTRY(stere) {
	ks0 = *(*param)("dk", "1.0");
	phits = *(*param)("rlat_ts", "90.");
	return(setup(inverse));
}
ENTRY(ups) {
	/* International Ellipsoid */
	phi0 = *(int *)(*param)("bsouth", "") ? - HALFPI: HALFPI;
	if (!es)
		emess(1,"Elliptical usage of UPS required");
	ks0 = .994;
	x0 = 2000000.;
	y0 = 2000000.;
	phits = HALFPI;
	lam0 = 0.;
	return(setup(inverse));
}
