#ifndef lint
static char *SCCSID = "@(#)SuppFuncs.c	USGS v.3.2";
#endif
# include "projects.h"
/* General short support functions, mostly for elliptical cases */

/* meridinal distance for ellipsoid and inverse
**	8th degree - accurate to < 1e-5 meters when used in conjuction
**		with typical major axis values.
**	Inverse determines phi to EPS (1e-11) radians, about 1e-6 seconds.
*/
# define C00 1.
# define C02 .25
# define C04 .046875
# define C06 .01953125
# define C08 .01068115234375
# define C22 .75
# define C44 .46875
# define C46 .01302083333333333333
# define C48 .00712076822916666666
# define C66 .36458333333333333333
# define C68 .00569661458333333333
# define C88 .3076171875
# define EPS 1e-11
# define MAX_ITER 10
	static double
en[5];
	void
enfn_() { double t;
	en[0] = C00 - es * (C02 + es * (C04 + es * (C06 + es * C08)));
	en[1] = es * (C22 - es * (C04 + es * (C06 + es * C08)));
	en[2] = (t = es * es) * (C44 - es * (C46 + es * C48));
	en[3] = (t *= es) * (C66 - es * C68);
	en[4] = t * es * C88;
}
	double
mlfn_(Phi, sphi, cphi) double Phi, sphi, cphi; {
	cphi *= sphi;
	sphi *= sphi;
	return(en[0] * Phi - cphi * (en[1] + sphi*(en[2]
		+ sphi*(en[3] + sphi*en[4]))));
}
	double
inv_mlfn_(arg) double arg; {
	double s, t, Phi, k = 1.-es;
	int i;

	Phi = arg;
	for (i = MAX_ITER; i ; --i) { /* rarely goes over 5 iterations */
		s = sin(Phi);
		t = 1. - es * s * s;
		t = (mlfn_(Phi, s, cos(Phi)) - arg) / ( k * t * sqrt(t));
		Phi -= t;
		if (fabs(t) < EPS)
			break;
	}
	return Phi;
}
/* determine constant small m */
	double
msfn_(sinphi, cosphi) double sinphi, cosphi; {
	return (cosphi / sqrt (1. - es * sinphi * sinphi));
}
/* determine small q */
# define EPSILON 1.0e-7
	double
qsfn_ (sinphi) double sinphi; {
	double con;

	if (e >= EPSILON) {
		con = e * sinphi;
		return (one_es * (sinphi / (1. - con * con) -
		   (.5 / e) * log ((1. - con) / (1. + con))));
	} else
		return (sinphi + sinphi);
}
/* determine small t */
	double
tsfn_ (Phi, sinphi) double Phi, sinphi; {
	sinphi *= e;
	return (tan (.5 * (HALFPI - Phi)) /
	   pow((1. - sinphi) / (1. + sinphi), .5 * e));
}
/* determine latitude from authalic latitude */

# define P00 .33333333333333333333
# define P01 .17222222222222222222
# define P02 .10257936507936507936
# define P10 .06388888888888888888
# define P11 .06640211640211640211
# define P20 .01641501294219154443
	static double
AP[3];
	void
authset() {
	double t;

	AP[0] = es * P00;
	t = es * es;
	AP[0] += t * P01;
	AP[1] = t * P10;
	t *= es;
	AP[0] += t * P02;
	AP[1] += t * P11;
	AP[2] = t * P20;
}
	double
authlat(beta) double beta; {
	double t = beta+beta;
	return(beta + AP[0] * sin(t) + AP[1] * sin(t+t) + AP[2] * sin(t+t+t));
}
/* reduce argument to range +/- PI */
/* note: PI adjusted high
** approx. true val:	3.14159265358979323844
*/
# define SPI		3.14159265359
	double
adjlon (lon) double lon; {
	while ( fabs(lon) > SPI )
		lon += lon < 0. ? TWOPI : -TWOPI;
	return( lon );
}
