/* generates 'T' option output */
#ifndef lint
static char RCSID[] = "@(#)$Id: gen_cheby.c,v 4.4 1992/07/14 01:42:12 gie Exp $";
#endif
#define __PJ_LIB
#include "projects.h"
#include <string.h>

static void
#ifdef __STDC__
put_coeff(double res, char *fmt, UV **F, int NU, int NV)
#else
put_coeff(res, fmt, F, NU, NV)
    double res;
    char *fmt;
    UV **F;
    int NU, NV;
#endif
{
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
	(void)printf("u: %d\n",++max+1);
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
	(void)printf("v: %d\n",++max+1);
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
	(void)printf("# u,v sums: %g %g, |u,v sums|: %g %g\n",
		sum.u,sum.v,asum.u,asum.v);
}

int
#ifdef __STDC__
gen_cheby(int inverse, UV proj(), char *s, PJ *P, int iargc, char **iargv) 
#else
gen_cheby(inverse, proj, s, P, iargc, iargv) 
    int inverse;
    UV (*proj)();
    char *s;
    PJ *P;
    int iargc;
    char **iargv;
#endif
{
	int NU = 15, NV = 15, i, res = -1, errin = 0;
	char *arg, fmt[30];
	UV low, upp, **F;
	double (*input)();
	int bchgen PROTO((UV, UV, int, int, UV **, UV(*)(UV)));

	(void)printf("#proj_Chebyshev\n#    run-line:\n");
	if (iargc > 0) { /* proj execution audit trail */
		for( i = 0 ; iargc ; --iargc) {
			arg = *iargv++;
			if (*arg != '+') {
				if (!i) putchar('#');
				i += printf(" %s",arg);
				if (i > 50) { putchar('\n'); i = 0; }
			}
		}
		if (i) putchar('\n');
	}
	(void)printf("# projection parameters\n");
	pj_pr_list(P);
	input = inverse ? strtod : dmstor;
	if (s && (s = strtok(s, ",")) && *s) low.u = input(s, 0); else ++errin;
	if (s && (s = strtok(0, ",")) && *s) upp.u = input(s, 0); else ++errin;
	if (s && (s = strtok(0, ",")) && *s) low.v = input(s, 0); else ++errin;
	if (s && (s = strtok(0, ",")) && *s) upp.v = input(s, 0); else ++errin;
	if (errin)
		emess(16,"null or absent -T parameters");

	/* WARNING strtol is not declared   -dpg */
	if (s && (s = strtok(0, ",")) && *s) res = strtol(s, 0, 10);
	if (s && (s = strtok(0, ",")) && *s) NU = strtol(s, 0, 10);
	if (s && (s = strtok(0, ",")) && *s) NV = strtol(s, 0, 10);
	if (low.u == upp.u || low.v >= upp.v)
		emess(16,"approx. argument range error");
	if (low.u > upp.u)
		low.u -= TWOPI;
	if (NU < 2 || NV < 2)
		emess(16,"approx. work dimensions (%d %d) too small",NU,NV);
	/* setup 2-D work array */
	if (!(F = (UV **)malloc(sizeof(UV *) * NU)))
memoryerr:	emess(2, "approx. memory allocation failure");
	for (i = 0; i < NU ; ++i)
		if (!(F[i] = (UV *)malloc(sizeof(UV) * NV))) goto memoryerr;
	bchgen(low, upp, NU, NV, F, proj);
	(void)printf("%c,%.12g,%.12g,%.12g,%.12g,%.12g\n ",inverse?'I':'F',
		P->lam0*RAD_TO_DEG,
		low.u*(inverse?1.:RAD_TO_DEG),upp.u*(inverse?1.:RAD_TO_DEG),
		low.v*(inverse?1.:RAD_TO_DEG),upp.v*(inverse?1.:RAD_TO_DEG));
	if (res <= 0)
		(void)sprintf(fmt," %%.%df",-res+1);
	else
		(void)strcpy(fmt," %.0f");
	put_coeff(pow(10.,(double)res), fmt, F, NU, NV);
	(void)printf("#end_proj_Chebyshev\n");
	/* clean up work arrays */
	for (i = 0; i < NU ; ++i)
		free(F[i]);
	free(F);
	return 0;
}
