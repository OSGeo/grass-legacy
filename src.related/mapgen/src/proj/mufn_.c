static char *SCCSID = "@(#)mufn_.c	AMG v.1.2";
/* rectifying latitude */
# include <math.h>
# define C1	1.5
# define C2	.84375
# define C3	1.3125
# define C4	1.71875
# define C5	1.57291666666666666666
# define C6	2.142578125
	void
mifn_(ua, es) double *ua, es; {
	double e2;

	es = sqrt(1. - es);
	es = (1. - es) / (1. + es);
	e2 = es * es;
	*ua++ = C1 * es - C2 * es * e2;
	*ua++ = e2 * (C3 - C4 * e2);
	*ua++ = C5 * es * e2;
	*ua = e2 * e2 * C6;
}
	double
mufn_(ua, u) double *ua, u; {
	double sum = u, t;
	u += u;
	sum += ua[0] * sin(u) + ua[1] * sin(t = u + u);
	return(sum + ua[2] * sin(u + t) + ua[3] * sin(t + t));
}
