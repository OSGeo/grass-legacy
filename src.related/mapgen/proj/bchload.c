#ifndef lint
static char *SCCSID = "@(#)bchload.c	USGS v.3.2";
#endif
/* Load standard proj -T file and provide Chebyshev evaluation
** Entry points:
**	(int)bchload(FILE *fid)	to load control and coefficients
**		returns 1 if inverse, 0 if forward
**	(UV)bcheval(UV input)	compute transformation
**
**	requires user supplied procedure emess() and adjlon()
*/
# include <stdio.h>
# include <math.h>
# define DEG_TO_RAD	.0174532925199432958
# define NEAR_ONE	1.0000000001
typedef struct { double u, v; }		UV;
	static UV 
w2, w, A, S,		/* local globals for evaluation */
a, b;			/* range of arguments */
	static
struct COEF {		/* coefficient structure */
	int m;		/* number of c coefficients */
	double *c;	/* Chebyshev coefficients */
} *cu, *cv;
	static int
mu, mv;			/* maximum cu and cv index */
# define MAX_LINE  50
	extern double
adjlon();
	static double
cmp,
dzero = 0.;
	static int
inverse;
	static void /* sets scaling parameters */
bchseval() {
	if (!inverse) {
		cmp = .5 * (a.u + b.u);
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

		/* check if current line has tag */
	if (fscanf(fid,"%15s",line) != 1 || strcmp(line,"#proj_Chebyshev"))
		emess(16,"invalid header");
	line_flush(fid);
		/* skip comment lines */
	while ((c = getc(fid)) == '#') line_flush(fid);
	if (c < 0) emess(16,"unexpected EOF");
	(void)ungetc(c, fid); /* went too far, backup */
		/* get range parameters */
	if (fscanf(fid,"%c,%le,%le,%le,%le,%le",&inverse,
		&cm,&a.u,&b.u,&a.v,&b.v) != 6)
		emess(16,"error on range line");
		/* retrieve coefficients */
	if (fscanf(fid,"%2s %d", line, &mu) != 2 || strcmp(line, "u:"))
		emess(16,"missing \"u:\" data");
	cu = lload(--mu, fid);
	if (fscanf(fid,"%2s %d", line, &mv) != 2 || strcmp(line, "v:"))
		emess(16,"missing \"v:\" data");
	cv = lload(--mv, fid);
		/* initialize */
	if (!(inverse = inverse == 'I' ? 1 : 0)) {
		a.u *= DEG_TO_RAD;
		a.v *= DEG_TO_RAD;
		b.u *= DEG_TO_RAD;
		b.v *= DEG_TO_RAD;
	}
	bchseval();
		/* check that trailer tag correct */
	for (;;) {
		line_flush(fid);
		if ((c = getc(fid)) != '#' || c < 0)
			emess(16,"no trailer tag");
		if (fscanf(fid,"%18s",line) == 1 && !strcmp(line,
			"end_proj_Chebyshev"))
			break;
	}
	line_flush(fid); /* leave at begining of next line after tag */
	return inverse;
}
	static double /* basic bivariate Chebyshev evaluation */
ceval(C, n) struct COEF *C; {
	double d=0, dd=0, vd, vdd, tmp, *c;
	int j;

	for (C += n ; n-- ; --C ) {
		if (j = C->m) {
			vd = vdd = 0.;
			for (c = C->c + --j; j ; --j ) {
				vd = w2.v * (tmp = vd) - vdd + *c--;
				vdd = tmp;
			}
			d = w2.u * (tmp = d) - dd + w.v * vd - vdd + 0.5 * *c;
		} else
			d = w2.u * (tmp = d) - dd;
		dd = tmp;
	}
	if (j = C->m) {
		vd = vdd = 0.;
		for (c = C->c + --j; j ; --j ) {
			vd = w2.v * (tmp = vd) - vdd + *c--;
			vdd = tmp;
		}
		return (w.u * d - dd + 0.5 * ( w.v * vd - vdd + 0.5 * *c ));
	} else
		return (w.u * d - dd);
}
	UV /* bivariate Chebyshev polynomial entry point */
bcheval(in) UV in; {
	UV out;

	if (!inverse)
		in.u = adjlon(in.u);
		/* scale to +-1 and check */
 	if (	fabs(w.u = ( in.u + in.u - A.u ) * S.u) > NEAR_ONE ||
 		fabs(w.v = ( in.v + in.v - A.v ) * S.v) > NEAR_ONE ) {
		out.u = HUGE;
	} else { /* double evaluation */
		w2.u = w.u + w.u;
		w2.v = w.v + w.v;
		out.u = ceval(cu, mu);
		out.v = ceval(cv, mv);
	}
	return out;
}
