#ifndef lint
static char RCSID[] = "@(#)$Id: pj_mlfn.c,v 4.1 1992/04/03 13:35:37 gie Exp $";
#endif
#include <math.h>
/* meridinal distance for ellipsoid and inverse
**	8th degree - accurate to < 1e-5 meters when used in conjuction
**		with typical major axis values.
**	Inverse determines phi to EPS (1e-11) radians, about 1e-6 seconds.
*/
#define C00 1.
#define C02 .25
#define C04 .046875
#define C06 .01953125
#define C08 .01068115234375
#define C22 .75
#define C44 .46875
#define C46 .01302083333333333333
#define C48 .00712076822916666666
#define C66 .36458333333333333333
#define C68 .00569661458333333333
#define C88 .3076171875
#define EPS 1e-11
#define MAX_ITER 10
#define EN_SIZE 5
double *
#ifdef __STDC__
pj_enfn(double es)
#else
pj_enfn(es)
    double es;
#endif
{
	double t, *en;

	if (en = (double *)malloc(EN_SIZE * sizeof(double))) {
		en[0] = C00 - es * (C02 + es * (C04 + es * (C06 + es * C08)));
		en[1] = es * (C22 - es * (C04 + es * (C06 + es * C08)));
		en[2] = (t = es * es) * (C44 - es * (C46 + es * C48));
		en[3] = (t *= es) * (C66 - es * C68);
		en[4] = t * es * C88;
	} /* else return NULL if unable to allocate memory */
	return en;
}

double
#ifdef __STDC__
pj_mlfn(double phi, double sphi, double cphi, double *en)
#else
pj_mlfn(phi, sphi, cphi, en)
    double phi, sphi, cphi, *en;
#endif
{
	cphi *= sphi;
	sphi *= sphi;
	return(en[0] * phi - cphi * (en[1] + sphi*(en[2]
		+ sphi*(en[3] + sphi*en[4]))));
}

double
#ifdef __STDC__
pj_inv_mlfn(double arg, double es, double *en)
#else
pj_inv_mlfn(arg, es, en)
    double arg, es, *en;
#endif
{
	double s, t, phi, k = 1.-es;
	int i;

	phi = arg;
	for (i = MAX_ITER; i ; --i) { /* rarely goes over 5 iterations */
		s = sin(phi);
		t = 1. - es * s * s;
		t = (pj_mlfn(phi, s, cos(phi), en) - arg) / ( k * t * sqrt(t));
		phi -= t;
		if (fabs(t) < EPS)
			break;
	}
	return phi;
}
