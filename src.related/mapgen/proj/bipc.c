static char *SCCSID = "@(#)bipc.c	AMG v.1.1";
/* Bipolar conic of western hemisphere */
# define	PROJ_MOD
# include	"projects.h"
struct _PROJ { PHEAD;
	int	NOSKEW;
};
# define noskew	proj->NOSKEW
# define EPS	1e-10
# define EPS10	1e-10
# define NITER	10
# define lamB	-.34894976726250681539
# define n	.63055844881274687180
# define F	1.89724742567461030582
# define Azab	.81650043674686363166
# define Azba	1.82261843856185925133
# define T	1.27246578267089012270
# define rhoc	1.20709121521568721927
# define cAzc	.69691523038678375519
# define sAzc	.71715351331143607555
# define C45	.70710678118654752469
# define S45	.70710678118654752410
# define C20	.93969262078590838411
# define S20	-.34202014332566873287
# define R110	1.91986217719376253360
# define R104	1.81514242207410275904
FORWARD(s_forward); /* spheroid */
	double cphi, sphi, tphi, t, al, Az, z, Av, cdlam, sdlam, r;
	int tag;

	cphi = cos(phi);
	sphi = sin(phi);
	cdlam = cos(sdlam = lamB - lam);
	sdlam = sin(sdlam);
	if (fabs(fabs(phi) - HALFPI) < EPS10) {
		Az = phi < 0. ? PI : 0.;
		tphi = HUGE;
	} else {
		tphi = sphi / cphi;
		Az = atan2(sdlam , C45 * (tphi - cdlam));
	}
	if (tag = (Az > Azba)) {
		cdlam = cos(sdlam = lam + R110);
		sdlam = sin(sdlam);
		z = acos(S20 * sphi + C20 * cphi * cdlam);
		if (tphi != HUGE)
			Az = atan2(sdlam, (C20 * tphi - S20 * cdlam));
		Av = Azab;
		y = rhoc;
	} else {
		z = acos(S45 * (sphi + cphi * cdlam));
		Av = Azba;
		y = -rhoc;
	}
	r = F * (t = pow(tan(.5 * z), n));
	al = acos((t + pow(tan(.5 * (R104 - z)), n)) / T);
	if (fabs(t = n * (Av - Az)) < al)
		r /= cos(al + (tag ? t : -t));
	x = r * sin(t);
	y += (tag ? -r : r) * cos(t);
	if (noskew) {
		t = x;
		x = -x * cAzc - y * sAzc; 
		y = -y * cAzc + t * sAzc; 
	}
	return (&xy);
}
INVERSE(s_inverse); /* spheroid */
	double t, r, rp, rl, al, z, fAz, Az, s, c, Av;
	int neg, i;

	if (noskew) {
		t = x;
		x = -x * cAzc + y * sAzc; 
		y = -y * cAzc - t * sAzc; 
	}
	if (neg = (x < 0.)) {
		y = rhoc - y;
		s = S20;
		c = C20;
		Av = Azab;
	} else {
		y += rhoc;
		s = S45;
		c = C45;
		Av = Azba;
	}
	rl = rp = r = hypot(x, y);
	fAz = fabs(Az = atan2(x, y));
	for (i = NITER; i ; --i) {
		z = 2. * atan(pow(r / F,1 / n));
		al = acos((pow(tan(.5 * z), n) +
		   pow(tan(.5 * (R104 - z)), n)) / T);
		if (fAz < al)
			r = rp * cos(al + (neg ? Az : -Az));
		if (fabs(rl - r) < EPS)
			break;
		rl = r;
	}
	if (! i) ERROR;
	Az = Av - Az / n;
	phi = asin(s * cos(z) + c * sin(z) * cos(Az));
	lam = atan2(sin(Az), c / tan(z) - s * cos(Az));
	if (neg)
		lam -= R110;
	else
		lam = lamB - lam;
	return (&lp);
}
ENTRY(bipc) {
	if (! proj) return (sizeof(struct _PROJ));
	if (comset(I_XY, proj, param)) ERROR;
	noskew = *(int *)(*param)("bns");
	proj->forward = s_forward;
	proj->inverse = s_inverse;
	return (1);
}
