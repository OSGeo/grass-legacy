#ifndef lint
static char *SCCSID = "@(#)gen_cheby.c	USGS v.3.1";
#endif
# include "projects.h"
	static void
put_coeff(res, fmt, F, NU, NV) double res; char *fmt; UV **F; {
	int j, i, max, k, l;
	UV asum, sum, *f;
	double t;

	/* produce error statistic for dropped points */
	asum.u = asum.v = sum.u = sum.v = 0.;
	for (j = 0; j < NU; ++j)
		for (f = F[j], i = 0; i < NV; ++i, ++f) {
			if ((t = fabs(f->u)) < res) {
				sum.u += f->u;
				asum.u += t;
				f->u = 0.;
			}
			if ((t = fabs(f->v)) < res) {
				sum.v += f->v;
				asum.v += t;
				f->v = 0.;
			}
		}
	for (j = 0, max = NU - 1; max >= 0 && !j ; --max)
		for (f = F[max]+(j = NV); j && !(--f)->u; --j) ;
	printf("u: %d\n",++max+1);
	l = 0;
	for (i = 0; i <= max; ++i) {
		for (f = F[i]+(k = NV); k && !(--f)->u; --k) ;
		if (k) {
			l = printf("%d %d",i,k);
			for (f = F[i], j = 0; j < k; ++j) {
				if (t = (f++)->u) {
					if (l > 65) { putchar('\n'); l = 0; }
					l += printf(fmt,t);
				} else
					l += printf(" 0");
			}
			putchar('\n');
		}
	}
	for (j = 0, max = NU - 1; max >= 0 && !j ; --max)
		for (f = F[max]+(j = NV); j && !(--f)->v ; --j);
	printf("v: %d\n",++max+1);
	l = 0;
	for (i = 0; i <= max; ++i) {
		for (f = F[i]+(k = NV); k && !(--f)->v; --k) ;
		if (k) {
			l = printf("%d %d",i,k);
			for (f = F[i], j = 0; j < k; ++j)
				if (t = (f++)->v) {
					if (l > 65) { putchar('\n'); l = 0; }
					l += printf(fmt,t);
				} else
					l += printf(" 0");
			putchar('\n');
		}
	}
	printf("# u,v sums: %g %g, |u,v sums|: %g %g\n",
		sum.u,sum.v,asum.u,asum.v);
}
	int
gen_cheby(inverse, proj, str) UV (*proj)(); char *str; {
	int NU = 15, NV = 15, i, res = -1;
	char *s = str, *arg, fmt[30];
	UV low, upp, **F, *malloc();
	long strtol();
	double (*input)(), strtod(), dmstor();
	extern int iargc;
	extern char **iargv;

	input = inverse ? strtod : dmstor;
	low.u = input(s, &s);
	if (*s++ == ',') upp.u = input(s, &s);
	if (*s++ == ',') low.v = input(s, &s);
	if (*s++ == ',') upp.v = input(s, &s);
	if (*s++ == ',') res = strtol(s, &s, 10);
	if (*s++ == ',') NU = strtol(s, &s, 10);
	if (*s++ == ',') NV = strtol(s, &s, 10);
	if (low.u == upp.u || low.v >= upp.v)
		emess(16,"approx. argument range error");
	if (low.u > upp.u)
		low.u -= TWOPI;
	if (NU < 2 || NV < 2)
		emess(16,"approx. work dimensions (%d %d) too small",NU,NV);
	if (!(F = (UV **)malloc(sizeof(UV *) * NU)))
memoryerr:	emess(2, "approx. memory allocation failure");
	for (i = 0; i < NU ; ++i)
		if (!(F[i] = (UV *)malloc(sizeof(UV) * NV))) goto memoryerr;
	bchgen(low, upp, NU, NV, F, proj);
	printf("#proj_Chebyshev\n#    run-line non +:\n");
	/* create audit trail, first run-line non + options  */
	for( i = 0 ; iargc ; --iargc)
		if (*(arg = *iargv++) != '+') {
			if (!i) putchar('#');
			i += printf(" %s",arg);
			if (i > 50) { putchar('\n'); i = 0; }
		}
	if (i) putchar('\n');
	printf("#    final cartographic parameters:\n");
	for( i = 0 ; dump_opt(&s, &arg); ) {
		if (!i) putchar('#');
		i += arg ? printf(" +%s=%s",s, arg) : printf(" +%s",s);
		if (i > 50) { putchar('\n'); i = 0; }
	}
	if (i) putchar('\n');
	printf("%c,%.12g,%.12g,%.12g,%.12g,%.12g\n ",inverse?'I':'F',
		lam0*RAD_TO_DEG,
		low.u*(inverse?1.:RAD_TO_DEG),upp.u*(inverse?1.:RAD_TO_DEG),
		low.v*(inverse?1.:RAD_TO_DEG),upp.v*(inverse?1.:RAD_TO_DEG));
	if (res <= 0)
		sprintf(fmt," %%.%df",-res+1);
	else
		strcpy(fmt,"%.0f");
	put_coeff(pow(10.,(double)res), fmt, F, NU, NV);
	printf("#end_proj_Chebyshev\n");
	return(0);
}
