static char *SCCSID = "@(#)authlat.c	AMB v.1.1";

# include <math.h>

/* determine latitude from authalic latitude */

# define P00 .33333333333333333333
# define P01 .17222222222222222222
# define P02 .10257936507936507936
# define P10 .06388888888888888888
# define P11 .06640211640211640211
# define P20 .01641501294219154443

	static double
t;
	void
authset(p, e2) double *p, e2; {

	p[0] = e2 * P00;
	t = e2 * e2;
	p[0] += t * P01;
	p[1] = t * P10;
	t *= e2;
	p[0] += t * P02;
	p[1] += t * P11;
	p[2] = t * P20;
}
	double
authlat(p, beta) double *p, beta; {
	t = beta+beta;
	return(beta + p[0] * sin(t) + p[1] * sin(t+t) + p[2] * sin(t+t+t));
}
