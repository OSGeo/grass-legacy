#ifndef lint
static char *SCCSID = "@(#)cnp.c	AMG v.3.1";
#endif
/* cnp - chebyshev polynomial coefficients
**	't' vector to be filled with Ti(x), i=0,...,'n'
*/
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
