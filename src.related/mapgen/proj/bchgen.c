#ifndef lint
static char *SCCSID = "@(#)bchgen.c	USGS v.3.2";
#endif
/* generate double bivariate Chebychev polynomial */
# include <math.h>
# define PI		3.14159265358979323846
typedef struct { double u, v; }	UV;
	extern void
emess();
	static char
errmess[] = "bchgen memory allocation failure";
	int
bchgen(a, b, nu, nv, f, func) UV a, b, **f, (*func)(); {
	int i, j, k;
	UV arg, *t, bma, bpa, *c, *malloc();
	double d, fac;

	bma.u = 0.5 * (b.u - a.u); bma.v = 0.5 * (b.v - a.v);
	bpa.u = 0.5 * (b.u + a.u); bpa.v = 0.5 * (b.v + a.v);
	for ( i = 0; i < nu; ++i) {
		arg.u = cos(PI * (i + 0.5) / nu) * bma.u + bpa.u;
		for ( j = 0; j < nv; ++j) {
			arg.v = cos(PI * (j + 0.5) / nv) * bma.v + bpa.v;
			f[i][j] = (*func)(arg);
			if ((f[i][j]).u == HUGE)
				return(1);
		}
	}
	if (!(c = malloc(nu * sizeof(UV)))) emess(16,errmess);
	fac = 2. / nu;
	for ( j = 0; j < nv ; ++j) {
		for ( i = 0; i < nu; ++i) {
			arg.u = arg.v = 0.;
			for (k = 0; k < nu; ++k) {
				d = cos(PI * i * (k + .5) / nu);
				arg.u += f[k][j].u * d;
				arg.v += f[k][j].v * d;
			}
			arg.u *= fac;
			arg.v *= fac;
			c[i] = arg;
		}
		for (i = 0; i < nu; ++i)
			f[i][j] = c[i];
	}
	free(c);
	if (!(c = malloc(nv * sizeof(UV)))) emess(16,errmess);
	fac = 2. / nv;
	for ( i = 0; i < nu; ++i) {
		t = f[i];
		for (j = 0; j < nv; ++j) {
			arg.u = arg.v = 0.;
			for (k = 0; k < nv; ++k) {
				d = cos(PI * j * (k + .5) / nv);
				arg.u += t[k].u * d;
				arg.v += t[k].v * d;
			}
			arg.u *= fac;
			arg.v *= fac;
			c[j] = arg;
		}
		f[i] = c;
		c = t;
	}
	free(c);
	return(0);
}
