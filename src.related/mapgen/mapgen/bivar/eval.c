#ifndef lint
static char *SCCSID = "@(#)eval.c	AMG v.3.2";
#endif
#include "bivar.h"
static double vprod();
	double
eval(npts, Cxy, nC) double *Cxy; {
	double vprod(), *q, v, max, fabs();

	q = bfacts.xyzw;
	max = 0.;
	while (npts--) {
		/* form univariate vectors */
		(*bfacts.np)(bfacts.Tx, *q++, bfacts.deg);
		(*bfacts.np)(bfacts.Ty, *q++, bfacts.deg);

		/* form bivariate vector */
		v = fabs(vprod(bfacts.deg, nC, bfacts.Tx, bfacts.Ty, Cxy)
			- *q++);
		if (v > max)
			max = v;
		if (bfacts.wghts > 3) q++;
	}
	return max;
}

static double
vprod(n, nC, Tx, Ty, Cxy)
double *Tx, *Ty, *Cxy;
{
	register double v;
	register i, j;

	v = 0.;
	for (i = 0; i <= n ; i++) {
		for (j = 0; j <= i; j++) {
			if (nC-- < 1) return v;
			if (*Cxy) v += Tx[i-j] * Ty[j] * *Cxy;
			Cxy++;
		}
	}
	return( v );
}
