#ifndef lint
static char *SCCSID = "@(#)UVlaea.c	USGS v.3.2";
#endif
/* Lambert Azimuthal Equal Area projection */
# include	"projects.h"
	static double
sinb1, cosb1, xmf, ymf, mmf, qp, d, rq;
	static int
mode;
# define sinph0	sinb1
# define cosph0	cosb1
# define EPS10	1.e-10
# define NITER	20
# define CONV	1.e-10
# define N_POLE	0
# define S_POLE	1
# define EQUIT	2
# define OBLIQ	3
#ifdef FOR_CODE
FORWARD(e_forward); /* ellipsoid */
	double coslam, sinlam, sinphi, q, sinb, cosb, b;

	coslam = cos(lam);
	sinlam = sin(lam);
	sinphi = sin(phi);
	q = qsfn_(sinphi);
	if (mode == OBLIQ || mode == EQUIT) {
		sinb = q / qp;
		cosb = sqrt(1. - sinb * sinb);
	}
	switch (mode) {
	case OBLIQ:
		b = 1. + sinb1 * sinb + cosb1 * cosb * coslam;
		break;
	case EQUIT:
		b = 1. + cosb * coslam;
		break;
	case N_POLE:
		b = HALFPI + phi;
		q = qp - q;
		break;
	case S_POLE:
		b = phi - HALFPI;
		q = qp + q;
		break;
	}
	if (fabs(b) < EPS10) F_ERROR;
	switch (mode) {
	case OBLIQ:
		y = ymf * ( b = sqrt(2. / b) )
		   * (cosb1 * sinb - sinb1 * cosb * coslam);
		goto eqcon;
		break;
	case EQUIT:
		y = (b = sqrt(2. / (1. + cosb * coslam))) * sinb * ymf; 
eqcon:
		x = xmf * b * cosb * sinlam;
		break;
	case N_POLE:
	case S_POLE:
		if (q >= 0.) {
			x = (b = sqrt(q)) * sinlam;
			y = coslam * (mode == S_POLE ? b : -b);
		} else
			x = y = 0.;
		break;
	}
	return (xy);
}
FORWARD(s_forward); /* spheroid */
	double  coslam, cosphi, sinphi;

	sinphi = sin(phi);
	cosphi = cos(phi);
	coslam = cos(lam);
	switch (mode) {
	case EQUIT:
		y = 1. + cosphi * coslam;
		goto oblcon;
	case OBLIQ:
		y = 1. + sinph0 * sinphi + cosph0 * cosphi * coslam;
oblcon:
		if (y <= EPS10) F_ERROR;
		x = (y = sqrt(2. / y)) * cosphi * sin(lam);
		y *= mode == EQUIT ? sinphi :
		   cosph0 * sinphi - sinph0 * cosphi * coslam;
		break;
	case N_POLE:
		coslam = -coslam;
	case S_POLE:
		if (fabs(phi + phi0) < EPS10) F_ERROR;
		y = FORTPI - phi * .5;
		y = 2. * (mode == S_POLE ? cos(y) : sin(y));
		x = y * sin(lam);
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
	double sinphi, cCe, sCe, q, rho, dphi, ab;
	extern errno;
	int i;

	errno = 0;
	switch (mode) {
	case EQUIT:
	case OBLIQ:
		if ((rho = hypot(x /= d, y *=  d)) < EPS10) {
			lam = 0.;
			phi = phi0;
			return (lp);
		}
		cCe = cos(sCe = 2. * asin(.5 * rho / rq));
		x *= (sCe = sin(sCe));
		if (mode == OBLIQ) {
			q = qp * (ab = cCe * sinb1 + y * sCe * cosb1 / rho);
			y = rho * cosb1 * cCe - y * sinb1 * sCe;
		} else {
			q = qp * (ab = y * sCe / rho);
			y = rho * cCe;
		}
		break;
	case N_POLE:
		y = -y;
	case S_POLE:
		if (!(q = (x * x + y * y)) ) {
			lam = 0.;
			phi = phi0;
			return (lp);
		}
		/*
		q = qp - q;
		*/
		ab = 1. - q / qp;
		if (mode == S_POLE)
			ab = - ab;
		break;
	}
	lam = atan2(x, y);
	phi = authlat(asin(ab));
	if (errno)
		I_ERROR;
	return (lp);
}
INVERSE(s_inverse); /* spheroid */
	double  coslam, cosphi, cosz, rh, sinz;

	rh = hypot(x, y);
	if ((phi = rh * .5 ) > 1.) I_ERROR;
	phi = 2. * asin(phi);
	if (mode == OBLIQ || mode == EQUIT) {
		sinz = sin(phi);
		cosz = cos(phi);
	}
	switch (mode) {
	case EQUIT:
		phi = fabs(rh) <= EPS10 ? 0. : asin(y * sinz / rh);
		x *= sinz;
		y = cosz * rh;
		break;
	case OBLIQ:
		phi = fabs(rh) <= EPS10 ? phi0 :
		   asin(cosz * sinph0 + y * sinz * cosph0 / rh);
		x *= sinz * cosph0;
		y = (cosz - sin(phi) * sinph0) * rh;
		break;
	case N_POLE:
		y = -y;
		phi = HALFPI - phi;
		break;
	case S_POLE:
		phi -= HALFPI;
		break;
	}
	lam = (y == 0. && (mode == EQUIT || mode == OBLIQ)) ?
		0. : atan2(x, y);
	return (lp);
}
#else
NULL_INVERSE(s_inverse);
NULL_INVERSE(e_inverse);
#endif
ENTRY(laea) {
	double t;

	if (fabs((t = fabs(phi0)) - HALFPI) < EPS10)
		mode = phi0 < 0. ? S_POLE : N_POLE;
	else if (fabs(t) < EPS10)
		mode = EQUIT;
	else
		mode = OBLIQ;
	if (es) {
		double X, sinphi;

		e = sqrt(es);
		qp = qsfn_(1.);
		mmf = .5 / (1. - es);
		authset();
		switch (mode) {
		case N_POLE:
		case S_POLE:
			d = 1.;
			break;
		case EQUIT:
			d = 1. / (rq = sqrt(.5 * qp));
			xmf = 1.;
			ymf = .5 * qp;
			break;
		case OBLIQ:
			rq = sqrt(.5 * qp);
			sinphi = sin(phi0);
			sinb1 = qsfn_(sinphi) / qp;
			cosb1 = sqrt(1. - sinb1 * sinb1);
			d = cos(phi0) / (sqrt(1. - es * sinphi * sinphi) *
			   rq * cosb1);
			ymf = (xmf = rq) / d;
			xmf *= d;
			break;
		}
		if (inverse) RETURN(e_inverse); else RETURN(e_forward);
	} else {
		if (mode == OBLIQ) {
			sinph0 = sin(phi0);
			cosph0 = cos(phi0);
		}
		if (inverse) RETURN(s_inverse); else RETURN(s_forward);
	}
}
