static char *SCCSID = "@(#)laea.c	AMG v.1.4";

# include <stdio.h>

/* Lambert Azimuthal Equal Area projection */
# define	PROJ_MOD
# include	"projects.h"
struct _PROJ { PHEAD;
	double PHI0, E, SINB1, COSB1, XMF, YMF, MMF, QP, D, RQ, AE[AE_MAX];
	int MODE;
};
# define EPS10	1.e-10
# define NITER	20
# define CONV	1.e-10
# define sinb1	proj->SINB1
# define cosb1	proj->COSB1
# define sinph0	proj->SINB1
# define cosph0	proj->COSB1
# define xmf	proj->XMF
# define ymf	proj->YMF
# define mmf	proj->MMF
# define qp	proj->QP
# define d	proj->D
# define rq	proj->RQ
# define ae	proj->AE
# define mode	proj->MODE
# define N_POLE	0
# define S_POLE	1
# define EQUIT	2
# define OBLIQ	3
FORWARD(e_forward); /* ellipsoid */
	double coslam, sinlam, sinphi, q, sinb, cosb, b;

	coslam = cos(lam);
	sinlam = sin(lam);
	sinphi = sin(phi);
	q = qsfn_(e, sinphi);
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
	if (fabs(b) < EPS10) ERROR;
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
	return (&xy);
}
INVERSE(e_inverse); /* ellipsoid */
	double sinphi, cCe, sCe, q, rho, dphi, ab;
	int i;

	errno = 0;
	switch (mode) {
	case EQUIT:
	case OBLIQ:
		if ((rho = hypot(x /= d, y *=  d)) < EPS10) {
			lam = 0.;
			phi = phi0;
			return (&lp);
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
			return (&lp);
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
	phi = authlat(ae, asin(ab));
	if (errno)
		ERROR;
	return (&lp);
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
		if (y <= EPS10) ERROR;
		x = (y = sqrt(2. / y)) * cosphi * sin(lam);
		y *= mode == EQUIT ? sinphi :
		   cosph0 * sinphi - sinph0 * cosphi * coslam;
		break;
	case N_POLE:
		coslam = -coslam;
	case S_POLE:
		if (fabs(phi + phi0) < EPS10) ERROR;
		y = FORTPI - phi * .5;
		y = 2. * (mode == S_POLE ? cos(y) : sin(y));
		x = y * sin(lam);
		y *= coslam;
		break;
	}
	return (&xy);
}
INVERSE(s_inverse); /* spheroid */
	double  coslam, cosphi, cosz, rh, sinz;

	rh = hypot(x, y);
	if ((phi = rh * .5 ) > 1.) ERROR;
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
	return (&lp);
}
ENTRY(laea) {
	double t;

	if (! proj) return (sizeof(struct _PROJ));
	if (comset(I_ALL, proj, param)) ERROR;
	phi0 = *(*param)("rlat_0", "");
	if (fabs((t = fabs(phi0)) - HALFPI) < EPS10)
		mode = phi0 < 0. ? S_POLE : N_POLE;
	else if (fabs(t) < EPS10)
		mode = EQUIT;
	else
		mode = OBLIQ;
	if (es) {
		double X, sinphi;

		proj->forward = e_forward;
		proj->inverse = e_inverse;
		e = sqrt(es);
		qp = qsfn_(e, 1.);
		mmf = .5 / (1. - es);
		authset(ae, es);
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
			sinb1 = qsfn_(e, sinphi) / qp;
			cosb1 = sqrt(1. - sinb1 * sinb1);
			d = cos(phi0) / (sqrt(1. - es * sinphi * sinphi) *
			   rq * cosb1);
			ymf = (xmf = rq) / d;
			xmf *= d;
			break;
		}
	} else {
		proj->forward = s_forward;
		proj->inverse = s_inverse;
		if (mode == OBLIQ) {
			sinph0 = sin(phi0);
			cosph0 = cos(phi0);
		}
	}
	return (1);
}
