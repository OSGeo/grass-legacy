#ifndef lint
static char *SCCSID = "@(#)bchload.c	USGS v.3.1";
#endif
/* load standard proj -T file and provide Chebyshev evaluation */
# include <stdio.h>
# include <math.h>
# define DEG_TO_RAD	.0174532925199432958
typedef struct { double u, v; }		UV;
	static UV 
y2, y, A, S,
a, b;			/* range of arguments */
	static
struct COEF {		/* coefficient structure */
	int m;		/* maximum c index */
	double *c;	/* Chebyshev coefficients */
} *cu, *cv;
	static int
mu, mv;			/* maximum cu and cv index */
# define MAX_LINE  50
	extern double
adjlon();
	static double
cm,
dzero = 0.;
	static int
inverse;
	static void /* sets scaling parameters */
bchseval() {
	if (!inverse) {
		cm = .5 * (a.u + b.u);
		a.u = adjlon(a.u);
		b.u = adjlon(b.u);
	}
	A.u = a.u + b.u;
	A.v = a.v + b.v;
	S.u = 1./( b.u - a.u);
	S.v = 1./( b.v - a.v);
}
	static struct COEF
*lload(n, fid) FILE *fid; {
	int i, j, ip, m;
	double *c;
	struct COEF *ct, *malloc();

	if (!(ct = malloc((n+1) * sizeof(struct COEF))))
memfail:	emess(16,"memory allocation failure");
	for (i = 0; i <= n ; ++i) {
		if (fscanf(fid,"%d %d",&ip,&m) != 2)
			emess(16,"failed to read row ncol");
		while (i < ip) {
			ct[i].m = 0;
			ct[i].c = &dzero;
			++i;
			if (i > n)
				emess(16,"index overflow");
		}
		ct[i].m = m;
		if (!(c = ct[i].c = (double *)malloc(m * sizeof(double))))
			goto memfail;
		for (j = 0; j < m; ++j)
			if (fscanf(fid,"%le",c++) != 1)
				emess(16,"coefficient read error");
	}
	return ct;
}
	static void
line_flush(fid) FILE *fid; {
	int c;

	while ((c = getc(fid)) != '\n' && c >= 0) ;
	if (c < 0)
		emess(16,"unexpected EOF");
}
	int
bchload(fid) FILE *fid; {
	int c;
	double cm;
	char line[MAX_LINE];

	if (fscanf(fid,"%15s",line) != 1 || strcmp(line,"#proj_Chebyshev"))
		emess(16,"invalid header");
	line_flush(fid);
	while ((c = getc(fid)) == '#')
		line_flush(fid);
	if (c < 0)
		emess(16,"unexpected EOF");
	ungetc(c, fid);
	if (fscanf(fid,"%c,%le,%le,%le,%le,%le",&inverse,
		&cm,&a.u,&b.u,&a.v,&b.v) != 6)
		emess(16,"error on range line");
	if (fscanf(fid,"%2s %d", line, &mu) != 2 || strcmp(line, "u:"))
		emess(16,"missing \"u:\" data");
	cu = lload(--mu, fid);
	if (fscanf(fid,"%2s %d", line, &mv) != 2 || strcmp(line, "v:"))
		emess(16,"missing \"v:\" data");
	cv = lload(--mv, fid);
	if (!(inverse = inverse == 'I' ? 1 : 0)) {
		a.u *= DEG_TO_RAD;
		a.v *= DEG_TO_RAD;
		b.u *= DEG_TO_RAD;
		b.v *= DEG_TO_RAD;
	}
	bchseval();
	for (;;) {
		line_flush(fid);
		if ((c = getc(fid)) != '#' || c < 0)
			emess(16,"no trailer tag");
		if (fscanf(fid,"%18s",line) == 1 && !strcmp(line,
			"end_proj_Chebyshev"))
			break;
	}
	line_flush(fid);
	return inverse;
}
	static double
ceval(C, n) struct COEF *C; {
	double d=0, dd=0, vd, vdd, tmp, *c;
	int j;

	for (C += n ; n-- ; --C ) {
		if (j = C->m) {
			vd = vdd = 0.;
			for (c = C->c + --j; j ; --j ) {
				vd = y2.v * (tmp = vd) - vdd + *c--;
				vdd = tmp;
			}
			d = y2.u * (tmp = d) - dd + y.v * vd - vdd + 0.5 * *c;
		} else
			d = y2.u * (tmp = d) - dd;
		dd = tmp;
	}
	if (j = C->m) {
		vd = vdd = 0.;
		for (c = C->c + --j; j ; --j ) {
			vd = y2.v * (tmp = vd) - vdd + *c--;
			vdd = tmp;
		}
		return (y.u * d - dd + 0.5 * ( y.v * vd - vdd + 0.5 * *c ));
	} else
		return (y.u * d - dd);
}
	UV /* evaluate bivariate Chebyshev polynomial */
bcheval(x) UV x; {
	UV res;

	if (!inverse)
		x.u = adjlon(x.u);
 	if (fabs(y.u = ( x.u + x.u - A.u ) * S.u) > 1.000000001) {
		res.u = HUGE;
		return (res);
	}
 	if (fabs(y.v = ( x.v + x.v - A.v ) * S.v) > 1.000000001) {
		res.u = HUGE;
		return (res);
	}
 	y2.u = y.u + y.u;
 	y2.v = y.v + y.v;
	res.u = ceval(cu, mu);
	res.v = ceval(cv, mv);
	return res;
}
