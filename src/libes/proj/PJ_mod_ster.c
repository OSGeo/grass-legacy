/*  Modified Stereographic projections */
#ifndef lint
static char RCSID[] = "@(#)$Id: PJ_mod_ster.c,v 4.2 1992/07/14 01:27:41 gie Exp $";
#endif
/* based upon Snyder and Linck, USGS-NMD */
#define __PROJ_PARMS \
    double	*A, *B; \
	double	cchio, schio; \
	int		n;
#define __PJ_LIB
#include	"projects.h"
#define EPSLN 1e-10

FORWARD(e_forward) { XY xy;  /* ellipsoid */
	double *Ap, *Bp, sinlon, coslon, esphi, chi, schi, cchi, g,
		s, xp, yp, r, ai, ar, bi, br, ain, arn;
    int j;

	sinlon = sin(lp.lam);
	coslon = cos(lp.lam);
	esphi = P->e * sin(lp.phi);
	chi = 2. * atan(tan((HALFPI + lp.phi) * .5) *
		pow((1. - esphi) / (1. + esphi), P->e * .5)) - HALFPI;
	schi = sin(chi);
	cchi = cos(chi);
	g = P->schio * schi + P->cchio * cchi * coslon;
	s = 2. / (1. + g);
	xp = s * cchi * sinlon;
	yp = s * (P->cchio * schi - P->schio * cchi * coslon);
	r = xp + xp;
	s = xp * xp + yp * yp;
	Ap = P->A;
	Bp = P->B;
	ar = *Ap;
	ai = *Bp;
	br = *--Ap;
	bi = *--Bp;
	for (j = 2; j <= P->n; ++j) {
	    arn = br + r * ar;
	    ain = bi + r * ai;
	    if (j != P->n) {
	    	br = *--Ap - s * ar;
	    	bi = *--Bp - s * ai;
	    	ar = arn;
	    	ai = ain;
		}
	}
	br = -s * ar;
	bi = -s * ai;
	ar = arn;
	ai = ain;
	xy.x = xp * ar - yp * ai + br;
	xy.y = yp * ar + xp * ai + bi;
	return xy;
}
INVERSE(e_inverse) { LP lp;  /* ellipsoid */
	int nn, j;
	double *Ap, *Bp, r, s, ar, ai, br, bi, cr, ci, dr, di, arn, ain, fxyr,
		fxyi, den, dxp, dyp, ds, rh, z, sinz, cosz, chi, phi, dphi, esphi,
		xp, yp, crn, cin, fpxyr, fpxyi;

	xp = xy.x;
	yp = xy.y;
	for (nn = 20; nn ;--nn) {
		r = xp + xp;
		s = xp * xp + yp * yp;
		Ap = P->A;
		Bp = P->B;
		ar = *Ap;
		ai = *Bp;
		br = *--Ap;
		bi = *--Bp;
		cr = P->n * ar;
		ci = P->n * ai;
		dr = (P->n - 1) * br;
		di = (P->n - 1) * bi;
		for (j = 2; j <= P->n; ++j) {
		    arn = br + r * ar;
		    ain = bi + r * ai;
		    if (j != P->n) {
			    br = *--Ap - s * ar;
			    bi = *--Bp - s * ai;
			    ar = arn;
			    ai = ain;
			    crn = dr + r * cr;
			    cin = di + r * ci;
			    dr = (P->n - j) * *Ap - s * cr;
			    di = (P->n - j) * *Bp - s * ci;
			    cr = crn;
			    ci = cin;
			}
		}
		br = -s * ar;
		bi = -s * ai;
		ar = arn;
		ai = ain;
		fxyr = xp * ar - yp * ai + br - xy.x;
		fxyi = yp * ar + xp * ai + bi - xy.y;
		fpxyr = xp * cr - yp * ci + dr;
		fpxyi = yp * cr + xp * ci + di;
		den = fpxyr * fpxyr + fpxyi * fpxyi;
		dxp = -(fxyr * fpxyr + fxyi * fpxyi) / den;
		dyp = -(fxyi * fpxyr - fxyr * fpxyi) / den;
		xp += dxp;
		yp += dyp;
		ds = fabs(dxp) + fabs(dyp);
		if (ds <= EPSLN)
			break;
	}
	if (nn) {
		rh = sqrt(xp * xp + yp * yp);
		z = 2. * atan(.5 * rh);
		sinz = sin(z);
		cosz = cos(z);
		lp.lam = P->lam0;
		if (fabs(rh) <= EPSLN) {
			lp.phi = P->phi0;
			return lp;
		}
		chi = aasin(cosz * P->schio + yp * sinz * P->cchio / rh);
		phi = chi;
		for (nn = 20; nn ;--nn) {
			esphi = P->e * sin(phi);
			dphi = 2. * atan(tan((HALFPI + chi) * .5) *
				pow((1. + esphi) / (1. - esphi), P->e * .5)) - HALFPI - phi;
			phi += dphi;
			if (fabs(dphi) <= EPSLN)
				break;
		}
	}
	if (nn) {
		lp.phi = phi;
		lp.lam = atan2(xp * sinz, rh * P->cchio * cosz - yp * 
			P->schio * sinz);
    } else
		lp.lam = lp.phi = HUGE_VAL;
	return lp;
}
FREEUP {  if (P) free(P); }

static PJ *
#ifdef __STDC__
setup(PJ *P)
#else
setup(P)
    PJ *P;
#endif
{ /* general initialization */
	double esphi, chio;
	int i;

	if (P->es) {
	esphi = P->e * sin(P->phi0);
	chio = 2. * atan(tan((HALFPI + P->phi0) * .5) *
		pow((1. - esphi) / (1. + esphi), P->e * .5)) - HALFPI;
	} else
		chio = P->phi0;
	P->schio = sin(chio);
	P->cchio = cos(chio);
	i = P->n - 1;
	P->A += i; P->B += i;
	P->pfree = freeup; P->inv = e_inverse; P->fwd = e_forward;
	return P;
}
ENTRY(pj_alsk) {
	static double /* Alaska ellipsoid */
Ae[] = {.9945303, .0052083, .0072721, -.0151089, .0642675, .3582802},
Be[] = {0., -.0027404, .0048181, -.1932526, -.1381226, -.2884586};
	static double /* Alaska sphere */
As[] = {.9972523, .0052513, .0074606, -.0153783, .0636871, .3660976 },
Bs[] = {0., -.0041175, .0048125, -.1968253, -.1408027, -.2937382};

	if (!P)
		return((PJ *) malloc(sizeof(PJ)));
	P->n = 6;
	P->lam0 = DEG_TO_RAD * -152.;
	P->phi0 = DEG_TO_RAD * 64.;
	if (P->es) {
		P->A = Ae; P->B = Be;
	} else {
		P->A = As; P->B = Bs;
	}
	return (setup(P));
}
ENTRY(pj_gs50) {
	static double /* GS50 ellipsoid */
Ae[] = {.9827497, .0210669, -.1031415, -.0323337, .0502303, .0251805,
	-.0012315, .0072202, -.0194029, -.0210072},
Be[] = {0., .0053804, -.0571664, -.0322847, .1211983, .0895678,
	-.1416121, -.1317091, .0759677, .0834037};
	static double /* GS50 sphere */
As[] = {.9842990, .0211642, -.1036018, -.0329095, .0499471, .0260460,
	.0007388, .0075848, -.0216473, .0225161},
Bs[] = {0., .0037608, -.0575102, -.0320119, .1223335, .0899805,
	-.1435792, -.1334108, .0776645, .0853673};

	if (!P)
		return((PJ *) malloc(sizeof(PJ)));
	P->n = 10;
	P->lam0 = DEG_TO_RAD * -120.;
	P->phi0 = DEG_TO_RAD * 45.;
	if (P->es) {
		P->A = Ae; P->B = Be;
	} else {
		P->A = As; P->B = Bs;
	}
	return (setup(P));
}
