#ifndef lint
static char *SCCSID = "@(#)bieval2.c	AMG v.3.1";
#endif
/* bieval2 - double bivariate evaluation
** Note: 'Px' is primary control and 'Py' is principly employed
**	for coefficient count and values.
**	Range, transformation and polynomial are assumed
**	equal for both evaluations.
**
**	This version is mainly for axis transformations.
*/
#include "bieval.h"

bieval2(x, y, xp, yp, Px, Py) double x, y, *xp, *yp; BICOEF *Px, *Py; {
	double C;
	int i, j, n;

		/* check if in range */
	if (x < Px->x_min || x > Px->x_max ||
		y < Px->y_min || y > Px->y_max)
		return 1;

		/* form univariate vectors */
	(*Px->poly)(Px->Tx, x * Px->x_scale + Px->x_off, Px->x_deg);
	(*Px->poly)(Px->Ty, y * Px->y_scale + Px->y_off, Px->y_deg);

		/* evaluate product */
	for (*xp = *yp = 0., i = n = 0; i <= Px->x_deg ; i++)
		for (j = 0; j <= i; j++) {
			if (n < Px->maxC && (C = Px->Cxy[n++]))
				*xp += Px->Tx[i-j] * Px->Ty[j] * C;
			if (n < Py->maxC && (C = Py->Cxy[n++]))
				*yp += Px->Tx[i-j] * Px->Ty[j] * C;
		}
	return 0;
}
