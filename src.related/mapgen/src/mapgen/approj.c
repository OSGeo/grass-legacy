#ifndef lint
static char *SCCSID = "@(#)approj.c	AMG v.3.1";
#endif
/* double bivariate evaluation of projection */
# include <math.h>
# include "mapgen.h"

# define ONE_EPS	1.001

extern struct map_def def;

static double Tx[MAX_T_DEG+1], Ty[MAX_T_DEG+1];
	static /* chebyshev polynomial coefficients */
cnp(t, x, n) double t[]; register double x; {
	register i;

	t[0] = 1.;
	if ( n > 0 ) {
		t[1] = x;
		x += x;
		for ( i = 1 ; i < n ; ++i )
			t[i + 1] = x * t[i] - t[i - 1];
	}
}
approj(x, y, xp, yp) double x, y, *xp, *yp; {
	register double *Cx, *Cy;
	double c;
	register i, j;
	int n;

		/* reduce range & scale */
	n = 0;
	if (fabs(x = x * def.x_scale + def.x_off) > ONE_EPS)
		n = (x < 0. ? LEFT : RIGHT);
	if (fabs(y = y * def.y_scale + def.y_off) > ONE_EPS)
		n = (y < 0. ? BOTTOM : TOP);
	if (n)
		return n;
		/* form univariate vectors */
	cnp(Tx, x, def.x_deg);
	cnp(Ty, y, def.y_deg);
		/* evaluate product */
	Cx = def.xC;
	Cy = def.yC;
	for (*xp = *yp = 0., i = 0; i <= def.x_deg ; i++)
		for (j = 0; j <= i; j++) {
			if (*(short *)Cx || *(short *)Cy) {
				c = Tx[i-j] * Ty[j];
				if (*(short *)Cx)
					*xp += c * *Cx;
				if (*(short *)Cy)
					*yp += c * *Cy;
			}
			Cx++;
			Cy++;
		}
	return 0;
}
