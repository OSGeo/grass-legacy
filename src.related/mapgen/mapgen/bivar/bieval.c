#ifndef lint
static char *SCCSID = "@(#)bieval.c	AMG v.3.1";
#endif
#include "bieval.h"
bieval(x, y, z, P) double x, y, *z; BICOEF *P; {
	double C;
	int i, j, n;

		/* check if in range */
	if (x < P->x_min || x > P->x_max ||
		y < P->y_min || y > P->y_max)
		return 1;

		/* form univariate vectors */
	(*P->poly)(P->Tx, x * P->x_scale + P->x_off, P->x_deg);
	(*P->poly)(P->Ty, y * P->y_scale + P->y_off, P->y_deg);

		/* evaluate product */
	for (*z = 0., i = n = 0; i <= P->x_deg ; i++)
		for (j = 0; j <= i; j++) {
			if (n >= P->maxC)
				return 0;
			if (C = P->Cxy[n++])
				*z += P->Tx[i-j] * P->Ty[j] * C;
		}
	return 0;
}
