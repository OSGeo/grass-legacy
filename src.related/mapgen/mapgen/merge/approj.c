#ifndef lint
static char *SCCSID = "@(#)approj.c	OEMG v.1.1";
#endif
# include <math.h>
# include "gen.h"
# define ONE_EPS	1.001
	static
double Tx[MAX_T_DEG+1], Ty[MAX_T_DEG+1];
	void static /* chebyshev polynomial coefficients */
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
	int /* project geographic to cartesian */
approj(x, y, xp, yp) double x, y, *xp, *yp; {
	register double *Cx, *Cy;
	double c;
	register i, j;
	int n;

		/* reduce range & scale */
	n = 0;
	if (fabs(x = x * m_def.x_scale + m_def.x_off) > ONE_EPS)
		n = (x < 0. ? LEFT : RIGHT);
	if (fabs(y = y * m_def.y_scale + m_def.y_off) > ONE_EPS)
		n = (y < 0. ? BOTTOM : TOP);
	if (!n) {
		cnp(Tx, x, m_def.x_deg); /* form univariate vectors */
		cnp(Ty, y, m_def.y_deg);
		Cx = m_def.xC;		/* evaluate product */
		Cy = m_def.yC;
		for (*xp = *yp = 0., i = 0; i <= m_def.x_deg ; i++)
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
	}
	return (n);
}
